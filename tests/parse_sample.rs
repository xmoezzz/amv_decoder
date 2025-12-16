use std::path::Path;

use amv_decoder::amv::AmvFile;
use amv_decoder::decoder::AmvDecoder;
use anyhow::{Context, Result};

/// Integration test that parses `testcase/1.amv`.
#[test]
#[ignore]
fn parse_sample_1_amv() {
    let p = Path::new("testcase/1.amv");
    assert!(p.exists(), "place a sample file at testcase/1.amv");
    let mut amv = AmvFile::open(p).expect("open and parse");
    assert!(amv.header.frame_count > 0);
    println!("{:#?}", amv.header);
    amv.iter_frames().for_each(|f| println!("{:#?}", f.unwrap().tag));
}

#[test]
#[ignore]
fn decode_first_packet_1_amv() -> Result<()> {
    let p = Path::new("testcase/1.amv");
    if !p.exists() {
        return Ok(());
    }

    unsafe {
        std::env::set_var("AMV_TEST_OUT", "testcase/out");
    }

    // Parse container.
    let mut amv = AmvFile::open(p).context("open amv")?;
    let header = amv.header.clone();
    let qtables = amv.qtables.clone();
    let mode = amv.header.mode()?;

    // Create decoder (stateful across packets).
    let mut dec = AmvDecoder::new(header, mode, qtables).context("create decoder")?;

    // Read exactly one packet and decode it.
    let mut it = amv.iter_frames();
    let pkt = it
        .read_next()
        .context("read first packet")?
        .context("no frame packets in file")?;

    let frame = dec.decode_packet(&pkt).context("decode packet")?;
    let expected = frame.width as usize * frame.height as usize * 4;
    assert_eq!(frame.rgba.len(), expected, "unexpected RGBA buffer size");

    // Optional: write a quick PPM for manual inspection.
    // Example: AMV_TEST_OUT=out cargo test -- --ignored --nocapture
    if let Ok(out_dir) = std::env::var("AMV_TEST_OUT") {
        let out = Path::new(&out_dir);
        std::fs::create_dir_all(out).ok();

        let ppm = out.join(format!("frame_{:06}.ppm", frame.index));
        write_ppm_rgb(&ppm, frame.width, frame.height, &frame.rgba)?;
        eprintln!("wrote {:?}", ppm);
    }

    Ok(())
}


#[test]
#[ignore]
fn decode_all_packets_2_amv() -> Result<()> {
    let p = Path::new("testcase/1.amv");
    if !p.exists() {
        return Ok(());
    }

    // Always write outputs under testcase/out
    unsafe { 
        std::env::set_var("AMV_TEST_OUT", "testcase/out");
    }

    // Parse container.
    let mut amv = AmvFile::open(p).context("open amv")?;
    let header = amv.header.clone();
    let qtables = amv.qtables.clone();
    let mode = amv.header.mode()?;

    // Create decoder (stateful across packets).
    let mut dec = AmvDecoder::new(header, mode, qtables).context("create decoder")?;

    // Output dir.
    let out_dir = std::env::var("AMV_TEST_OUT").unwrap();
    let out = Path::new(&out_dir);
    std::fs::create_dir_all(out).ok();

    // Decode all packets until EOF.
    let mut it = amv.iter_frames();
    let mut n = 0usize;

    loop {
        let pkt_opt = it.read_next().context("read next packet")?;
        let Some(pkt) = pkt_opt else { break };

        let frame = dec.decode_packet(&pkt).with_context(|| format!("decode packet {}", n))?;
        let expected = frame.width as usize * frame.height as usize * 4;
        assert_eq!(
            frame.rgba.len(),
            expected,
            "unexpected RGBA buffer size at frame {}",
            n
        );

        let ppm = out.join(format!("frame_{:06}.ppm", n));
        write_ppm_rgb(&ppm, frame.width, frame.height, &frame.rgba)?;
        eprintln!("wrote {:?}", ppm);

        n += 1;
    }

    eprintln!("decoded {} frame(s)", n);
    Ok(())
}


fn write_ppm_rgb(path: &Path, width: u16, height: u16, rgba: &[u8]) -> Result<()> {
    use std::io::Write;

    let mut out = Vec::with_capacity((width as usize) * (height as usize) * 3 + 64);
    out.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
    for px in rgba.chunks_exact(4) {
        out.push(px[0]);
        out.push(px[1]);
        out.push(px[2]);
    }
    let mut f = std::fs::File::create(path)?;
    f.write_all(&out)?;
    Ok(())
}