use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Neg, Sub},
};

// Fixed point number in Q21.10 signed format
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FixedPoint(i32);

const FRACTION_BITS: u32 = 10;
const DIVISOR: i32 = 1 << FRACTION_BITS;
const DIVISOR_FLOAT: f64 = DIVISOR as f64;

const DECIMAL_FACTOR: u64 = 1000000000000;
const DECIMAL_MASK: u64 = DIVISOR as u64 - 1;

impl Display for FixedPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (sign, decimal) = if self.0 >= 0 {
            (false, self.0 as u64)
        } else {
            (true, -self.0 as u64)
        };
        let decimal = (decimal & DECIMAL_MASK) * DECIMAL_FACTOR / DIVISOR as u64;
        let decimal = format!("{:012}", decimal);
        let mut decimal_length = 0;
        for (i, c) in decimal.chars().enumerate() {
            if c != '0' {
                decimal_length = i;
            }
        }
        let decimal: String = decimal.chars().take(decimal_length + 1).collect();
        if sign {
            write!(f, "-{}.{}", -self.0 / DIVISOR, decimal)
        } else {
            write!(f, "{}.{}", self.0 / DIVISOR, decimal)
        }
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

impl From<i32> for FixedPoint {
    fn from(value: i32) -> Self {
        FixedPoint(value * DIVISOR)
    }
}

impl From<FixedPoint> for i32 {
    fn from(value: FixedPoint) -> Self {
        value.0 / DIVISOR
    }
}

impl From<i64> for FixedPoint {
    fn from(value: i64) -> Self {
        FixedPoint(value as i32 * DIVISOR)
    }
}

impl Add for FixedPoint {
    type Output = FixedPoint;

    fn add(self, rhs: Self) -> Self::Output {
        FixedPoint(self.0 + rhs.0)
    }
}

impl Sub for FixedPoint {
    type Output = FixedPoint;

    fn sub(self, rhs: Self) -> Self::Output {
        FixedPoint(self.0 - rhs.0)
    }
}

impl Mul for FixedPoint {
    type Output = FixedPoint;

    fn mul(self, rhs: Self) -> Self::Output {
        FixedPoint((self.0 * rhs.0) >> FRACTION_BITS)
    }
}

impl Div for FixedPoint {
    type Output = FixedPoint;

    fn div(self, rhs: Self) -> Self::Output {
        FixedPoint((self.0 << FRACTION_BITS) / rhs.0)
    }
}

impl Neg for FixedPoint {
    type Output = FixedPoint;

    fn neg(self) -> Self::Output {
        FixedPoint(-self.0)
    }
}
