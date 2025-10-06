use std::fmt::Display;

// Fixed point number in Q21.10 signed format
#[derive(Debug, Copy, Clone)]
pub struct FixedPoint(i32);

const FRACTION_BITS: u32 = 10;
const DIVISOR: i32 = 1 << FRACTION_BITS;
const DIVISOR_FLOAT: f64 = DIVISOR as f64;

const DECIMAL_FACTOR: u64 = 1000000000000;
const DECIMAL_MASK: u64 = DIVISOR as u64 - 1;

impl Display for FixedPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decimal = (self.0 as u64 & DECIMAL_MASK) * DECIMAL_FACTOR / DIVISOR as u64;
        let decimal_str = format!("{:012}", decimal);
        let mut decimal_length = 0;
        for (i, c) in decimal_str.chars().enumerate() {
            if c != '0' {
                decimal_length = i;
            }
        }
        let decimal_trimmed: String = decimal_str.chars().take(decimal_length + 1).collect();
        write!(f, "{}.{}", self.0 / DIVISOR, decimal_trimmed)
    }
}

impl From<f64> for FixedPoint {
    fn from(value: f64) -> Self {
        FixedPoint((value * DIVISOR_FLOAT) as i32)
    }
}

impl From<FixedPoint> for f64 {
    fn from(value: FixedPoint) -> Self {
        value.0 as f64 / DIVISOR_FLOAT
    }
}
