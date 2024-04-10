use bigdecimal::{BigDecimal, FromPrimitive, Signed, ToPrimitive, Zero};
use num_bigint::BigInt;

#[derive(Clone, Debug)]
pub enum SparqlNumber {
    NativeInt(isize),
    BigInt(BigInt),
    Decimal(BigDecimal),
    Float(f32),
    Double(f64),
    IllFormed,
}

impl From<isize> for SparqlNumber {
    fn from(value: isize) -> Self {
        SparqlNumber::NativeInt(value)
    }
}

impl From<BigInt> for SparqlNumber {
    fn from(value: BigInt) -> Self {
        SparqlNumber::BigInt(value)
    }
}

impl From<BigDecimal> for SparqlNumber {
    fn from(value: BigDecimal) -> Self {
        SparqlNumber::Decimal(value)
    }
}

impl From<f32> for SparqlNumber {
    fn from(value: f32) -> Self {
        SparqlNumber::Float(value)
    }
}

impl From<f64> for SparqlNumber {
    fn from(value: f64) -> Self {
        SparqlNumber::Double(value)
    }
}

macro_rules! impl_sparqlnumber_integer_from {
    ($t: ty) => {
        impl From<$t> for SparqlNumber {
            fn from(value: $t) -> Self {
                if let Ok(val) = value.try_into() {
                    SparqlNumber::NativeInt(val)
                } else {
                    SparqlNumber::BigInt(value.into())
                }
            }
        }
    };
}
impl_sparqlnumber_integer_from!(i64);
impl_sparqlnumber_integer_from!(i32);
impl_sparqlnumber_integer_from!(i16);
impl_sparqlnumber_integer_from!(i8);
impl_sparqlnumber_integer_from!(u64);
impl_sparqlnumber_integer_from!(u32);
impl_sparqlnumber_integer_from!(u16);
impl_sparqlnumber_integer_from!(u8);

impl SparqlNumber {
    pub fn parse_integer(lex: &str) -> Self {
        if let Ok(val) = lex.parse::<isize>() {
            val.into()
        } else if let Ok(val) = lex.parse::<BigInt>() {
            val.into()
        } else {
            return Self::IllFormed;
        }
    }

    pub fn parse<T: std::str::FromStr + Into<Self>>(lex: &str) -> Self {
        if let Ok(val) = lex.parse::<T>() {
            val.into()
        } else {
            Self::IllFormed
        }
    }

    pub fn check<F: FnOnce(&Self) -> bool>(self, predicate: F) -> Self {
        if predicate(&self) {
            self
        } else {
            Self::IllFormed
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_zero(),
            SparqlNumber::BigInt(inner) => inner.is_zero(),
            SparqlNumber::Decimal(inner) => inner.is_zero(),
            SparqlNumber::Float(inner) => inner.is_zero(),
            SparqlNumber::Double(inner) => inner.is_zero(),
            SparqlNumber::IllFormed => false,
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_positive(),
            SparqlNumber::BigInt(inner) => inner.is_positive(),
            SparqlNumber::Decimal(inner) => inner.is_positive(),
            SparqlNumber::Float(inner) => inner.is_positive(),
            SparqlNumber::Double(inner) => inner.is_positive(),
            SparqlNumber::IllFormed => false,
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(inner) => inner.is_negative(),
            SparqlNumber::BigInt(inner) => inner.is_negative(),
            SparqlNumber::Decimal(inner) => inner.is_negative(),
            SparqlNumber::Float(inner) => inner.is_negative(),
            SparqlNumber::Double(inner) => inner.is_negative(),
            SparqlNumber::IllFormed => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            SparqlNumber::NativeInt(i) => !i.is_zero(),
            SparqlNumber::BigInt(i) => !i.is_zero(),
            SparqlNumber::Decimal(d) => !d.is_zero(),
            SparqlNumber::Double(d) => !d.is_zero() && !d.is_nan(),
            SparqlNumber::Float(d) => !d.is_zero(),
            SparqlNumber::IllFormed => false,
        }
    }

    /// Coerce to a decimal
    ///
    /// ## Precondition
    /// Will panic if called on anything but a NativeInt or BigInt.
    /// NB: Decimal must not be coerced to decimal, as this would cause a clone.
    fn coerce_to_decimal(&self) -> BigDecimal {
        match self {
            SparqlNumber::NativeInt(inner) => BigDecimal::from_isize(*inner).unwrap(),
            SparqlNumber::BigInt(inner) => inner.clone().into(),
            _ => panic!(),
        }
    }

    /// Coerce to a f32
    ///
    /// ## Precondition
    /// Will panic if called on a Double or IllFormed.
    fn coerce_to_float(&self) -> f32 {
        match self {
            SparqlNumber::NativeInt(inner) => *inner as f32,
            SparqlNumber::BigInt(inner) => inner.to_f32().unwrap(),
            SparqlNumber::Decimal(inner) => inner.to_f32().unwrap(),
            SparqlNumber::Float(inner) => *inner,
            _ => panic!(),
        }
    }

    /// Coerce to a f64
    ///
    /// ## Precondition
    /// Will panic if called on a IllFormed.
    fn coerce_to_double(&self) -> f64 {
        match self {
            SparqlNumber::NativeInt(inner) => *inner as f64,
            SparqlNumber::BigInt(inner) => inner.to_f64().unwrap(),
            SparqlNumber::Decimal(inner) => inner.to_f64().unwrap(),
            SparqlNumber::Float(inner) => *inner as f64,
            SparqlNumber::Double(inner) => *inner,
            _ => panic!(),
        }
    }

    fn coercing_operator<F1, F2, F3, F4, F5, OI, O>(
        &self,
        rhs: &Self,
        fint: F1,
        fbig: F2,
        fdec: F3,
        fflt: F4,
        fdbl: F5,
    ) -> Option<O>
    where
        F1: FnOnce(isize, isize) -> Option<OI>,
        F2: FnOnce(&BigInt, &BigInt) -> Option<O>,
        F3: FnOnce(&BigDecimal, &BigDecimal) -> Option<O>,
        F4: FnOnce(f32, f32) -> Option<O>,
        F5: FnOnce(f64, f64) -> Option<O>,
        O: From<OI>,
    {
        use SparqlNumber::*;
        match (self, rhs) {
            (_, IllFormed) => None,
            (IllFormed, _) => None,
            //
            (Double(lhs), rhs) => fdbl(*lhs, rhs.coerce_to_double()),
            (lhs, Double(rhs)) => fdbl(lhs.coerce_to_double(), *rhs),
            //
            (Float(lhs), rhs) => fflt(*lhs, rhs.coerce_to_float()),
            (lhs, Float(rhs)) => fflt(lhs.coerce_to_float(), *rhs),
            //
            (Decimal(lhs), Decimal(rhs)) => fdec(lhs, rhs),
            (Decimal(lhs), rhs) => fdec(lhs, &rhs.coerce_to_decimal()),
            (lhs, Decimal(rhs)) => fdec(&lhs.coerce_to_decimal(), rhs),
            //
            (BigInt(lhs), BigInt(rhs)) => fbig(lhs, rhs),
            (NativeInt(lhs), BigInt(rhs)) => fbig(&(*lhs).into(), rhs),
            //
            (BigInt(lhs), NativeInt(rhs)) => fbig(lhs, &(*rhs).into()),
            (NativeInt(lhs), NativeInt(rhs)) => fint(*lhs, *rhs)
                .map(O::from)
                .or_else(|| fbig(&(*lhs).into(), &(*rhs).into())),
        }
    }
}

impl std::ops::Add for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn add(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_add,
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
            |x, y| Some((x + y).into()),
        )
    }
}

impl std::ops::Sub for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn sub(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_sub,
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
            |x, y| Some((x - y).into()),
        )
    }
}

impl std::ops::Mul for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn mul(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            isize::checked_mul,
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
            |x, y| Some((x * y).into()),
        )
    }
}

impl std::ops::Div for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn div(self, rhs: &'_ SparqlNumber) -> Self::Output {
        self.coercing_operator(
            rhs,
            |_, _| None as Option<i32>,
            |x, y| (!y.is_zero()).then(|| (BigDecimal::from(x.clone()) / BigDecimal::from(y.clone())).into()), // TODO this can probably be achieved with less clones
            |x, y| (!y.is_zero()).then(|| (x / y).into()),
            |x, y| Some((x / y).into()),
            |x, y| Some((x / y).into()),
        )
    }
}

impl std::ops::Neg for &'_ SparqlNumber {
    type Output = Option<SparqlNumber>;

    fn neg(self) -> Self::Output {
        match self {
            SparqlNumber::NativeInt(inner) => Some((-inner).into()),
            SparqlNumber::BigInt(inner) => Some((-inner).into()),
            SparqlNumber::Decimal(inner) => Some((-inner).into()),
            SparqlNumber::Float(inner) => Some((-inner).into()),
            SparqlNumber::Double(inner) => Some((-inner).into()),
            SparqlNumber::IllFormed => None,
        }
    }
}

impl std::cmp::PartialEq for &'_ SparqlNumber {
    fn eq(&self, other: &Self) -> bool {
        self.coercing_operator(
            other,
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
            |x, y| Some(x == y),
        )
        .unwrap_or(false)
    }
}

impl std::cmp::PartialOrd for &'_ SparqlNumber {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.coercing_operator(
            other,
            |x, y| x.partial_cmp(&y),
            |x, y| x.partial_cmp(&y),
            |x, y| x.partial_cmp(&y),
            |x, y| x.partial_cmp(&y),
            |x, y| x.partial_cmp(&y),
        )
    }
}
