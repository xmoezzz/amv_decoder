use std::f64::consts::PI;

/// A correctness-first 8x8 IDCT.
///
/// The original decoder likely uses an integer SIMD implementation. For reverse
/// engineering and frame dumping, this straightforward reference IDCT is
/// usually sufficient.
///
/// Input coefficients are expected to be *dequantized* and may be scaled by a
/// constant (the original engine shifts coefficients left by 2 before IDCT).
pub fn idct8x8(coeff: &[i32; 64]) -> [i16; 64] {
    let mut out = [0i16; 64];
    for y in 0..8 {
        for x in 0..8 {
            let mut sum = 0.0f64;
            for v in 0..8 {
                for u in 0..8 {
                    let cu = if u == 0 { 1.0 / 2f64.sqrt() } else { 1.0 };
                    let cv = if v == 0 { 1.0 / 2f64.sqrt() } else { 1.0 };
                    let c = coeff[v * 8 + u] as f64;
                    let a = ((2 * x + 1) as f64 * u as f64 * PI) / 16.0;
                    let b = ((2 * y + 1) as f64 * v as f64 * PI) / 16.0;
                    sum += cu * cv * c * a.cos() * b.cos();
                }
            }
            // JPEG IDCT has a 1/4 factor.
            let val = (sum / 4.0).round();
            let val = val.clamp(i16::MIN as f64, i16::MAX as f64) as i16;
            out[y * 8 + x] = val;
        }
    }
    out
}

/// For a DC-only block, the spatial domain is a constant: dc/8.
pub fn idct_dc_only(dc: i32) -> i16 {
    // Round-to-nearest (biased) division by 8.
    let v = if dc >= 0 { (dc + 4) >> 3 } else { -((-dc + 4) >> 3) };
    v.clamp(i16::MIN as i32, i16::MAX as i32) as i16
}
