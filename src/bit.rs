use anyhow::{bail, Result};

/// A simple MSB-first bit reader over a byte slice.
///
/// The original decoder reads 32-bit words and shifts left; this implementation
/// is intentionally straightforward and correct rather than micro-optimized.
pub struct BitReader<'a> {
    data: &'a [u8],
    byte_pos: usize,
    bit_pos: u8, // 0..7, where 0 means next bit is (byte >> 7)
}

impl<'a> BitReader<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, byte_pos: 0, bit_pos: 0 }
    }

    #[inline]
    pub fn bits_left(&self) -> usize {
        let bytes_left = self.data.len().saturating_sub(self.byte_pos);
        bytes_left.saturating_mul(8).saturating_sub(self.bit_pos as usize)
    }

    #[inline]
    fn read_bit(&mut self) -> Result<u32> {
        if self.byte_pos >= self.data.len() {
            bail!("bitstream underrun");
        }
        let b = self.data[self.byte_pos];
        let bit = (b >> (7 - self.bit_pos)) & 1;
        self.bit_pos += 1;
        if self.bit_pos == 8 {
            self.bit_pos = 0;
            self.byte_pos += 1;
        }
        Ok(bit as u32)
    }

    pub fn read_bits(&mut self, n: u8) -> Result<u32> {
        if n == 0 {
            return Ok(0);
        }
        if n > 24 {
            // More than 24 bits is not expected for baseline JPEG-like codes here.
            bail!("read_bits: unsupported n={}", n);
        }
        let mut v: u32 = 0;
        for _ in 0..n {
            v = (v << 1) | self.read_bit()?;
        }
        Ok(v)
    }

    /// Read `n` bits and interpret them as a signed value using JPEG's sign-extension rule.
    pub fn read_jpeg_signed(&mut self, n: u8) -> Result<i32> {
        if n == 0 {
            return Ok(0);
        }
        let raw = self.read_bits(n)? as i32;
        let half = 1i32 << (n - 1);
        if raw < half {
            Ok(raw - ((1i32 << n) - 1))
        } else {
            Ok(raw)
        }
    }
}
