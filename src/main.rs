use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

use amv_decoder::amv::{dump_packets, AmvFile, DumpOptions};
use amv_decoder::decoder::AmvDecoder;

#[derive(Parser)]
#[command(name = "amv_decoder")]
#[command(about = "Parse AJPM/Alpha Movie style files and split frame packets", long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Dump frame packets and segments to a directory
    Dump {
        /// Input .amv file path
        input: PathBuf,

        /// Output directory
        #[arg(long)]
        out: PathBuf,

        /// Maximum number of frames to dump (default: all frames in header, or until EOF)
        #[arg(long)]
        max_frames: Option<usize>,
    },

    /// Decode frame packets into RGBA frames (experimental)
    Decode {
        /// Input .amv file path
        input: PathBuf,

        /// Output directory
        #[arg(long)]
        out: PathBuf,

        /// Maximum number of frames to decode
        #[arg(long)]
        max_frames: Option<usize>,

        /// Also write a PPM file (RGB only) for quick viewing
        #[arg(long, default_value_t = true)]
        ppm: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.cmd {
        Command::Dump { input, out, max_frames } => {
            fs::create_dir_all(&out).with_context(|| format!("create out dir: {out:?}"))?;
            let opts = DumpOptions { max_frames };
            dump_packets(&input, &out, opts)?;
        }
        Command::Decode { input, out, max_frames, ppm } => {
            fs::create_dir_all(&out).with_context(|| format!("create out dir: {out:?}"))?;

            let mut amv = AmvFile::open(&input)?;
            let header = amv.header.clone();
            let qtables = amv.qtables.clone();
            let mode = amv.mode;
            let mut dec = AmvDecoder::new(header, mode, qtables)?;

            let mut count: usize = 0;
            for pkt in amv.iter_frames() {
                let pkt = pkt?;
                let frame = dec.decode_packet(&pkt)?;

                let base = format!("frame_{:06}", frame.index);
                let rgba_path = out.join(format!("{base}.rgba"));
                fs::write(&rgba_path, &frame.rgba)
                    .with_context(|| format!("write: {rgba_path:?}"))?;

                if ppm {
                    let ppm_path = out.join(format!("{base}.ppm"));
                    write_ppm_rgb(&ppm_path, frame.width, frame.height, &frame.rgba)
                        .with_context(|| format!("write: {ppm_path:?}"))?;
                }

                count += 1;
                if let Some(m) = max_frames {
                    if count >= m {
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

fn write_ppm_rgb(path: &PathBuf, width: u16, height: u16, rgba: &[u8]) -> Result<()> {
    use std::io::Write;

    let mut out = Vec::with_capacity((width as usize) * (height as usize) * 3 + 64);
    out.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
    for px in rgba.chunks_exact(4) {
        out.push(px[0]);
        out.push(px[1]);
        out.push(px[2]);
    }
    let mut f = fs::File::create(path)?;
    f.write_all(&out)?;
    Ok(())
}
