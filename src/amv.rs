use std::fmt;
use std::fs::{self, File};
use std::io::{self, BufReader, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use anyhow::{anyhow, bail, Context, Result};
use serde::{Deserialize, Serialize};

/// File magic `0x4D504A41` (little-endian bytes: 'A' 'J' 'P' 'M').
pub const MAGIC_AJPM: u32 = 0x4D504A41;

/// Decoder mode derived from the file header `attr` bits.
///
/// Observed behavior:
/// - if (attr & 1) != 0 -> mode B (a2+29 == 0) with 192-byte quant tables
/// - else (attr & 2) != 0 -> mode A (a2+29 != 0) with 128-byte quant tables
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AmvMode {
    /// Mode A: frame packet header is 24 bytes and contains an explicit `seg0_len`.
    A,
    /// Mode B: frame packet header is 20 bytes and payload length is `chunk_size - 12`.
    B,
}

impl fmt::Display for AmvMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AmvMode::A => write!(f, "A"),
            AmvMode::B => write!(f, "B"),
        }
    }
}

/// 40-byte file header.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AmvHeader {
    pub magic: u32,
    /// Unknown field at offset 0x04.
    pub unk04: u32,
    pub revision: u32,
    pub header_size: u32,
    /// Unknown field at offset 0x10 (present in loader stack layout; shifts subsequent fields by 4 bytes).
    pub unk10: u32,
    pub frame_count: u32,
    pub fps_num: u32,
    pub fps_den: u32,
    pub width: u16,
    pub height: u16,
    pub attr: u8,
    pub reserved: [u8; 3],
}

impl AmvHeader {
    pub fn mode(&self) -> Result<AmvMode> {
        // Mirrors loader logic: if (attr & 1) -> stream+29=0 else must have (attr & 2) -> stream+29=1
        if (self.attr & 1) != 0 {
            Ok(AmvMode::B)
        } else if (self.attr & 2) != 0 {
            Ok(AmvMode::A)
        } else {
            bail!("invalid attr: 0x{:02x}", self.attr);
        }
    }

    pub fn quant_table_size(&self) -> Result<usize> {
        let mode = self.mode()?;
        Ok(match mode {
            AmvMode::A => 128,
            AmvMode::B => 192,
        })
    }

    pub fn validate(&self) -> Result<()> {
        if self.magic != MAGIC_AJPM {
            bail!("bad magic: 0x{:08x} (expected 0x{:08x})", self.magic, MAGIC_AJPM);
        }
        if self.revision != 0 {
            bail!("unsupported revision: {}", self.revision);
        }
        if self.header_size != 168 && self.header_size != 232 {
            bail!("unexpected header_size: {} (expected 168 or 232)", self.header_size);
        }
        if self.frame_count == 0 {
            bail!("frame_count is zero");
        }
        if self.fps_num == 0 || self.fps_den == 0 {
            bail!("invalid fps: {}/{}", self.fps_num, self.fps_den);
        }
        if self.width == 0 || self.height == 0 {
            bail!("invalid width/height: {}x{}", self.width, self.height);
        }

        // Cross-check quant table size implied by header_size matches mode
        let qsize_by_header = (self.header_size as i64) - 40;
        let qsize_by_mode = self.quant_table_size()? as i64;
        if qsize_by_header != qsize_by_mode {
            bail!(
                "header_size implies qtables {} bytes, but mode implies {} bytes (attr=0x{:02x})",
                qsize_by_header,
                qsize_by_mode,
                self.attr
            );
        }

        Ok(())
    }
}

/// Global quant tables in file order.
/// Mode A: q0 + q1 (q2 is absent).
/// Mode B: q0 + q1 + q2.
// Quant tables are stored as Vec<u8> (length 64) for compatibility with older serde/toolchains that
// may not implement Serialize/Deserialize for fixed-size arrays > 32.
#[derive(Debug, Clone, Serialize)]
pub struct QuantTables {
    /// Quantization table 0 (64 bytes).
    pub q0: Vec<u8>,
    /// Quantization table 1 (64 bytes).
    pub q1: Vec<u8>,
    /// Optional quantization table 2 (64 bytes) for mode B.
    pub q2: Option<Vec<u8>>,
}
impl QuantTables {
    pub fn from_bytes(mode: AmvMode, bytes: &[u8]) -> Result<Self> {
        fn take64(bytes: &[u8], start: usize) -> Vec<u8> {
            bytes.iter().skip(start).take(64).copied().collect()
        }

        match mode {
            AmvMode::A => {
                if bytes.len() != 128 {
                    bail!("mode A expects 128 bytes of quant tables, got {}", bytes.len());
                }
                let q0 = take64(bytes, 0);
                let q1 = take64(bytes, 64);
                Ok(Self { q0, q1, q2: None })
            }
            AmvMode::B => {
                if bytes.len() != 192 {
                    bail!("mode B expects 192 bytes of quant tables, got {}", bytes.len());
                }
                let q0 = take64(bytes, 0);
                let q1 = take64(bytes, 64);
                let q2 = Some(take64(bytes, 128));
                Ok(Self { q0, q1, q2 })
            }
        }
    }
}

/// Frame packet kind.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum FramePacketKind {
    /// Mode A: a 24-byte header and two payload segments.
    A {
        header: FrameAHeader,
        seg0_len: u32,
        seg1_len: u32,
    },
    /// Mode B: a 20-byte header and a single payload blob.
    B {
        header: FrameBHeader,
        payload_len: u32,
    },
}

/// Parsed frame packet metadata plus raw bytes.
#[derive(Debug, Clone)]
pub struct FramePacket {
    pub index: u32,
    pub file_offset: u64,
    pub tag: u32,
    pub chunk_size: u32,
    pub raw_packet: Vec<u8>, // includes the full header (24 or 20) and payload bytes read
    pub kind: FramePacketKind,
    pub seg0: Option<Vec<u8>>, // Mode A only
    pub seg1: Option<Vec<u8>>, // Mode A only
}

impl FramePacket {
    /// Return the parsed Mode A header, if this packet is Mode A.
    pub fn header_a(&self) -> Option<FrameAHeader> {
        match &self.kind {
            FramePacketKind::A { header, .. } => Some(*header),
            _ => None,
        }
    }

    /// Return the parsed Mode B header, if this packet is Mode B.
    pub fn header_b(&self) -> Option<FrameBHeader> {
        match &self.kind {
            FramePacketKind::B { header, .. } => Some(*header),
            _ => None,
        }
    }

    /// For Mode B packets, return the payload bytes after the 20-byte header.
    pub fn payload_b(&self) -> Option<&[u8]> {
        match &self.kind {
            FramePacketKind::B { .. } => self.raw_packet.get(20..),
            _ => None,
        }
    }
}

/// Mode A (sub_10016D20) frame header: 24 bytes total.
/// Layout:
///   u32 tag
///   u32 chunk_size
///   u32 frame_id
///   i16 x
///   i16 y
///   u16 w
///   u16 h
///   u32 seg0_len
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct FrameAHeader {
    pub frame_id: u32,
    pub x: i16,
    pub y: i16,
    pub w: u16,
    pub h: u16,
}

/// Mode B (sub_10017570) frame header: 20 bytes total.
/// Layout:
///   u32 tag
///   u32 chunk_size
///   u32 frame_id
///   i16 p0
///   i16 p1
///   u16 w
///   u16 h
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct FrameBHeader {
    pub frame_id: u32,
    pub p0: i16,
    pub p1: i16,
    pub w: u16,
    pub h: u16,
}

/// Opened AMV file with a seekable reader.
#[derive(Debug, Clone)]
pub struct AmvFile<R: Read + Seek> {
    pub header: AmvHeader,
    pub mode: AmvMode,
    pub qtables: QuantTables,
    reader: R,
    first_frame_offset: u64,
}

impl AmvFile<BufReader<File>> {
    pub fn open(path: &Path) -> Result<Self> {
        let f = File::open(path).with_context(|| format!("open: {path:?}"))?;
        let reader = BufReader::new(f);
        Self::from_reader(reader)
    }
}

impl<R: Read + Seek> AmvFile<R> {
    pub fn from_reader(mut reader: R) -> Result<Self> {
        // Read header
        let mut buf = [0u8; 40];
        reader.read_exact(&mut buf).context("read file header (40 bytes)")?;
        let header = parse_header40(&buf)?;
        header.validate()?;
        let mode = header.mode()?;

        // Read global quant tables
        let qsize = header.quant_table_size()?;
        let mut qbuf = vec![0u8; qsize];
        reader.read_exact(&mut qbuf).context("read quant tables")?;
        let qtables = QuantTables::from_bytes(mode, &qbuf)?;

        // The loader seeks to header_size for first frame
        let first_frame_offset = header.header_size as u64;
        reader
            .seek(SeekFrom::Start(first_frame_offset))
            .context("seek to first frame offset")?;

        Ok(Self { header, mode, qtables, reader, first_frame_offset })
    }

    pub fn iter_frames(&mut self) -> FrameIter<&mut R> {
        FrameIter {
            reader: &mut self.reader,
            mode: self.mode,
            next_index: 0,
        }
    }
}

/// Frame iterator that reads packets sequentially from the current file position.
pub struct FrameIter<R: Read + Seek> {
    reader: R,
    mode: AmvMode,
    next_index: u32,
}

impl<R: Read + Seek> FrameIter<R> {
    pub fn read_next(&mut self) -> Result<Option<FramePacket>> {
        let start_off = self.reader.seek(SeekFrom::Current(0)).context("tell")?;

        // Try to read enough for the fixed header size. If EOF, return None.
        let fixed = match self.mode {
            AmvMode::A => 24,
            AmvMode::B => 20,
        };

        let mut hdr_bytes = vec![0u8; fixed];
        match self.reader.read_exact(&mut hdr_bytes) {
            Ok(()) => {}
            Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(anyhow!(e)).context("read frame header"),
        }

        match self.mode {
            AmvMode::A => {
                let (tag, chunk_size, header, seg0_len) = parse_frame_a_header(&hdr_bytes)?;
                // chunk_size includes 16 bytes of "chunk data header" after the initial 8 bytes (tag+chunk_size)
                // seg1_len = chunk_size - 16 - seg0_len
                if chunk_size < 16 {
                    bail!("frame {}: invalid chunk_size {} (<16)", self.next_index, chunk_size);
                }
                if seg0_len > chunk_size - 16 {
                    bail!(
                        "frame {}: seg0_len {} exceeds chunk_size-16 {}",
                        self.next_index,
                        seg0_len,
                        chunk_size - 16
                    );
                }
                let seg1_len = (chunk_size - 16) - seg0_len;

                validate_aligned16(header.w, header.h)
                    .with_context(|| format!("frame {}: w/h alignment", self.next_index))?;

                let mut seg0 = vec![0u8; seg0_len as usize];
                self.reader.read_exact(&mut seg0).context("read seg0")?;

                let mut seg1 = vec![0u8; seg1_len as usize];
                self.reader.read_exact(&mut seg1).context("read seg1")?;

                // Build raw packet bytes
                let mut raw_packet = Vec::with_capacity(fixed + seg0.len() + seg1.len());
                raw_packet.extend_from_slice(&hdr_bytes);
                raw_packet.extend_from_slice(&seg0);
                raw_packet.extend_from_slice(&seg1);

                let pkt = FramePacket {
                    index: self.next_index,
                    file_offset: start_off,
                    tag,
                    chunk_size,
                    raw_packet,
                    kind: FramePacketKind::A {
                        header,
                        seg0_len,
                        seg1_len,
                    },
                    seg0: Some(seg0),
                    seg1: Some(seg1),
                };

                self.next_index += 1;
                Ok(Some(pkt))
            }
            AmvMode::B => {
                let (tag, chunk_size, header) = parse_frame_b_header(&hdr_bytes)?;

                validate_aligned16(header.w, header.h)
                    .with_context(|| format!("frame {}: w/h alignment", self.next_index))?;

                // chunk_size includes 12 bytes of internal header (frame_id..w..h) after the initial 8 bytes
                if chunk_size < 12 {
                    bail!("frame {}: invalid chunk_size {} (<12)", self.next_index, chunk_size);
                }
                let payload_len = chunk_size - 12;

                let mut payload = vec![0u8; payload_len as usize];
                self.reader.read_exact(&mut payload).context("read payload")?;

                let mut raw_packet = Vec::with_capacity(fixed + payload.len());
                raw_packet.extend_from_slice(&hdr_bytes);
                raw_packet.extend_from_slice(&payload);

                let pkt = FramePacket {
                    index: self.next_index,
                    file_offset: start_off,
                    tag,
                    chunk_size,
                    raw_packet,
                    kind: FramePacketKind::B { header, payload_len },
                    seg0: None,
                    seg1: None,
                };

                self.next_index += 1;
                Ok(Some(pkt))
            }
        }
    }
}

// Allow idiomatic iteration:
//
//   for pkt in amv.iter_frames() {
//       let pkt = pkt?;
//       ...
//   }
//
// We use `Result<FramePacket>` as the iterator item so callers can handle parse errors.
impl<R: Read + Seek> Iterator for FrameIter<R> {
    type Item = Result<FramePacket>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_next() {
            Ok(Some(pkt)) => Some(Ok(pkt)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

fn validate_aligned16(w: u16, h: u16) -> Result<()> {
    if (w & 0xF) != 0 || (h & 0xF) != 0 {
        bail!("w/h not multiple of 16: {}x{}", w, h);
    }
    Ok(())
}

fn parse_header40(buf: &[u8; 40]) -> Result<AmvHeader> {
    // IMPORTANT: file is little-endian.
    //
    // IDA's loader reads 40 bytes into a local buffer that overlaps multiple locals.
    // There is an unnamed u32 at offset 0x10, so fields after header_size are shifted by +4
    // compared to a naïve contiguous struct guess.
    //
    // Offsets (bytes):
    // 0x00 magic
    // 0x04 unk04
    // 0x08 revision
    // 0x0C header_size
    // 0x10 unk10
    // 0x14 frame_count
    // 0x18 fps_num
    // 0x1C fps_den
    // 0x20 width (u16)
    // 0x22 height (u16)
    // 0x24 attr (u8)
    // 0x25..0x27 reserved (3 bytes)
    let magic = u32::from_le_bytes(buf[0..4].try_into().unwrap());
    let unk04 = u32::from_le_bytes(buf[4..8].try_into().unwrap());
    let revision = u32::from_le_bytes(buf[8..12].try_into().unwrap());
    let header_size = u32::from_le_bytes(buf[12..16].try_into().unwrap());
    let unk10 = u32::from_le_bytes(buf[16..20].try_into().unwrap());
    let frame_count = u32::from_le_bytes(buf[20..24].try_into().unwrap());
    let fps_num = u32::from_le_bytes(buf[24..28].try_into().unwrap());
    let fps_den = u32::from_le_bytes(buf[28..32].try_into().unwrap());
    let width = u16::from_le_bytes(buf[32..34].try_into().unwrap());
    let height = u16::from_le_bytes(buf[34..36].try_into().unwrap());
    let attr = buf[36];
    let reserved: [u8; 3] = buf[37..40].try_into().unwrap();

    Ok(AmvHeader {
        magic,
        unk04,
        revision,
        header_size,
        unk10,
        frame_count,
        fps_num,
        fps_den,
        width,
        height,
        attr,
        reserved,
    })
}

fn parse_frame_a_header(buf: &[u8]) -> Result<(u32, u32, FrameAHeader, u32)> {
    if buf.len() != 24 {
        bail!("mode A header must be 24 bytes, got {}", buf.len());
    }
    let tag = u32::from_le_bytes(buf[0..4].try_into().unwrap());
    let chunk_size = u32::from_le_bytes(buf[4..8].try_into().unwrap());
    let frame_id = u32::from_le_bytes(buf[8..12].try_into().unwrap());
    let x = i16::from_le_bytes(buf[12..14].try_into().unwrap());
    let y = i16::from_le_bytes(buf[14..16].try_into().unwrap());
    let w = u16::from_le_bytes(buf[16..18].try_into().unwrap());
    let h = u16::from_le_bytes(buf[18..20].try_into().unwrap());
    let seg0_len = u32::from_le_bytes(buf[20..24].try_into().unwrap());

    Ok((tag, chunk_size, FrameAHeader { frame_id, x, y, w, h }, seg0_len))
}

fn parse_frame_b_header(buf: &[u8]) -> Result<(u32, u32, FrameBHeader)> {
    if buf.len() != 20 {
        bail!("mode B header must be 20 bytes, got {}", buf.len());
    }
    let tag = u32::from_le_bytes(buf[0..4].try_into().unwrap());
    let chunk_size = u32::from_le_bytes(buf[4..8].try_into().unwrap());
    let frame_id = u32::from_le_bytes(buf[8..12].try_into().unwrap());
    let p0 = i16::from_le_bytes(buf[12..14].try_into().unwrap());
    let p1 = i16::from_le_bytes(buf[14..16].try_into().unwrap());
    let w = u16::from_le_bytes(buf[16..18].try_into().unwrap());
    let h = u16::from_le_bytes(buf[18..20].try_into().unwrap());

    Ok((tag, chunk_size, FrameBHeader { frame_id, p0, p1, w, h }))
}

#[derive(Debug, Clone)]
pub struct DumpOptions {
    pub max_frames: Option<usize>,
}

#[derive(Debug, Serialize)]
struct DumpIndex {
    header: AmvHeader,
    mode: AmvMode,
    qtables: QuantTables,
    frames: Vec<DumpFrame>,
    warnings: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DumpFrame {
    index: u32,
    file_offset: u64,
    tag: u32,
    chunk_size: u32,
    kind: FramePacketKind,
    files: Vec<String>,
}

/// Dump packets into `out_dir`.
pub fn dump_packets(input: &Path, out_dir: &Path, opts: DumpOptions) -> Result<()> {
    let mut amv = AmvFile::open(input)?;
    let mode = amv.mode;
    let header = amv.header.clone();
    let qtables = amv.qtables.clone();

    let frames_dir = out_dir.join("frames");
    fs::create_dir_all(&frames_dir).context("create frames dir")?;

    let mut it = amv.iter_frames();
    let max_frames = opts.max_frames.unwrap_or(header.frame_count as usize);

    let mut frames_meta: Vec<DumpFrame> = Vec::new();
    let mut warnings: Vec<String> = Vec::new();

    for _ in 0..max_frames {
        let pkt = match it.read_next()? {
            Some(p) => p,
            None => {
                warnings.push("EOF reached before frame_count".to_string());
                break;
            }
        };

        let base = format!("frame_{:06}", pkt.index);
        let mut files: Vec<String> = Vec::new();

        let packet_path = frames_dir.join(format!("{base}.packet.bin"));
        write_file(&packet_path, &pkt.raw_packet)?;
        files.push(rel(&packet_path, out_dir));

        if let (Some(seg0), Some(seg1)) = (pkt.seg0.as_ref(), pkt.seg1.as_ref()) {
            let seg0_path = frames_dir.join(format!("{base}.seg0.bin"));
            let seg1_path = frames_dir.join(format!("{base}.seg1.bin"));
            write_file(&seg0_path, seg0)?;
            write_file(&seg1_path, seg1)?;
            files.push(rel(&seg0_path, out_dir));
            files.push(rel(&seg1_path, out_dir));
        }

        frames_meta.push(DumpFrame {
            index: pkt.index,
            file_offset: pkt.file_offset,
            tag: pkt.tag,
            chunk_size: pkt.chunk_size,
            kind: pkt.kind,
            files,
        });
    }

    let idx = DumpIndex { header, mode, qtables, frames: frames_meta, warnings };
    let idx_path = out_dir.join("index.json");
    let mut wf = File::create(&idx_path).context("create index.json")?;
    let s = serde_json::to_string_pretty(&idx).context("json serialize")?;
    wf.write_all(s.as_bytes()).context("write index.json")?;

    Ok(())
}

fn write_file(path: &Path, data: &[u8]) -> Result<()> {
    let mut f = File::create(path).with_context(|| format!("create file: {path:?}"))?;
    f.write_all(data).with_context(|| format!("write file: {path:?}"))?;
    Ok(())
}

fn rel(path: &Path, base: &Path) -> String {
    path.strip_prefix(base)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}
