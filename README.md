# amv_decoder

`amv_decoder` is an experimental Rust project that parses and (partially) decodes the **AJPM / “Alpha Movie”** video
format used by some **KiriKiri2** and **KiriKiriZ (krkrz)** engine titles.

This repository is the product of reverse engineering work. It is not affiliated with, endorsed by, or supported by
any engine or game vendor.

## Status

- The container parser and packet splitter are stable enough for iterative RE work.

## Features

- Reads the 40-byte file header (little-endian, magic `0x4D504A41` / `AJPM`).
- Loads global quantization tables.
- Iterates frame packets from `header_size`.
- Dumps raw packet bytes and per-segment blobs for analysis.
- Decodes frame packets into RGBA frames (experimental) and can emit PPM images for quick inspection.

## Quick start

Prerequisites

- Rust stable toolchain.

Build

```bash
cargo build
```

Dump packets (recommended first step for new samples)

```bash
cargo run -- dump testcase/1.amv --out out_dump
```

This writes:

- `out_dump/index.json`
- `out_dump/frames/frame_000000.packet.bin`
- For packet type A (see format notes), also:
  - `out_dump/frames/frame_000000.seg0.bin`
  - `out_dump/frames/frame_000000.seg1.bin`

Decode frames (experimental)

```bash
cargo run -- decode testcase/1.amv --out out_decode --ppm true
```

This writes per-frame outputs:

- `out_decode/frame_000000.rgba` (raw RGBA bytes)
- `out_decode/frame_000000.ppm` (RGB-only PPM, if `--ppm true`)

## Tests

Sample-based decode tests are marked `#[ignore]` because `testcase/1.amv` is not committed.
Run ignored tests locally:

```bash
cargo test -- --ignored --nocapture
```

## AMV container notes

All integers are little-endian.

### File header (40 bytes)

A minimal working layout based on observed loader behavior:

- `u32 magic` = `0x4D504A41` (`AJPM`)
- `u32 unknown_04`
- `u32 revision` (expected `0`)
- `u32 header_size` (observed `168` or `232`)
- `u32 unknown_10`
- `u32 frame_count`
- `u32 fps_num`
- `u32 fps_den`
- `u16 width`
- `u16 height`
- `u8  attr`
- `u8[3] reserved`

Immediately after the 40-byte header, the file contains global quantization tables whose size is
`header_size - 40`:

- `128` bytes in packet type A mode
- `192` bytes in packet type B mode

The engine-side flag that selects the seek path (`stream+29` in the IDA output) is derived from `attr`.
In practice, this correlates with whether alpha data is present.

### Frame packet type A (seek path `sub_10016D20`)

This packet type begins with a 24-byte header and is split into two payload segments. Conceptually:

- `u32 tag`
- `u32 chunk_size` (bytes after the first 8 bytes)
- `u32 frame_id`
- `i16 p0`
- `i16 p1`
- `u16 w_aligned` (multiple of 16)
- `u16 h_aligned` (multiple of 16)
- `u32 seg0_len`
- `u8  seg0[seg0_len]`
- `u8  seg1[chunk_size - 16 - seg0_len]`

Observed behavior suggests:

- `seg0` is zlib-compressed and inflates to a `w_aligned * h_aligned` byte plane.
- `seg1` is an entropy-coded DCT coefficient stream.
- Per macroblock, 6 × 8×8 blocks are decoded (consistent with YCbCr 4:2:0 without alpha).

### Frame packet type B (seek path `sub_10017570`)

This packet type begins with a 20-byte header and contains a single payload blob:

- `u32 tag`
- `u32 chunk_size` (bytes after the first 8 bytes)
- `u32 frame_id`
- `i16 p0`
- `i16 p1`
- `u16 w_aligned` (multiple of 16)
- `u16 h_aligned` (multiple of 16)
- `u8  payload[chunk_size - 12]`

Observed behavior suggests:

- Per macroblock, 10 × 8×8 blocks are decoded.
- Blocks appear to be split across multiple Huffman/bitstream contexts.
- The 10-block structure is consistent with Y (4 blocks) + Cb (1) + Cr (1) + Alpha (4), but this remains a
  working hypothesis until composition code is fully matched.

### Entropy coding model (high level)

Both packet types use a JPEG-like coding model for 8×8 blocks:

- DC coefficient uses a predictor and Huffman-coded category, followed by extra bits.
- AC coefficients use Huffman-coded (RUN, SIZE) symbols with zig-zag order and EOB/ZRL semantics.
- Coefficients are dequantized using the global quantization tables and reconstructed via IDCT.
- The output is then composed into a frame buffer.

## License

MPL-2.0 (see `Cargo.toml`).
