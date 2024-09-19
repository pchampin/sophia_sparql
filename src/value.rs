use std::{cmp::Ordering, sync::Arc};

use bigdecimal::BigDecimal;
use sophia::{
    api::term::LanguageTag,
    term::{ArcTerm, GenericLiteral},
};

mod _xsd_date_time;
pub use _xsd_date_time::XsdDateTime;
mod _number;
pub use _number::SparqlNumber;

#[derive(Clone, Debug)]
pub enum SparqlValue {
    Number(SparqlNumber),
    String(Arc<str>, Option<LanguageTag<Arc<str>>>),
    Boolean(Option<bool>),
    DateTime(Option<XsdDateTime>),
}

impl SparqlValue {
    pub fn try_from_term(term: &ArcTerm) -> Option<Self> {
        if let ArcTerm::Literal(genlit) = term {
            Self::try_from_literal(genlit)
        } else {
            None
        }
    }

    pub fn try_from_literal(genlit: &GenericLiteral<Arc<str>>) -> Option<Self> {
        match genlit {
            GenericLiteral::LanguageString(lex, tag) => {
                Some(Self::String(lex.clone(), Some(tag.clone())))
            }
            GenericLiteral::Typed(lex, dt) => {
                let dt = dt.as_str();
                if !dt.starts_with(XSD) {
                    return None;
                }
                match &dt[XSD.len()..] {
                    "integer" => Some(Self::Number(SparqlNumber::parse_integer(lex))),
                    "decimal" => Some(Self::Number(SparqlNumber::parse::<BigDecimal>(lex))),
                    "float" => Some(Self::Number(SparqlNumber::parse::<f32>(lex))),
                    "double" => Some(Self::Number(SparqlNumber::parse::<f64>(lex))),
                    "string" => Some(Self::String(lex.clone(), None)),
                    "boolean" => Some(Self::Boolean(lex.parse().ok())),
                    "dateTime" => Some(Self::DateTime(lex.parse().ok())),
                    "nonPositiveInteger" => Some(Self::Number(
                        SparqlNumber::parse_integer(lex).check(|n| !n.is_positive()),
                    )),
                    "negativeInteger" => Some(Self::Number(
                        SparqlNumber::parse_integer(lex).check(|n| n.is_negative()),
                    )),
                    "long" => Some(Self::Number(SparqlNumber::parse::<i64>(lex))),
                    "int" => Some(Self::Number(SparqlNumber::parse::<i32>(lex))),
                    "short" => Some(Self::Number(SparqlNumber::parse::<i16>(lex))),
                    "byte" => Some(Self::Number(SparqlNumber::parse::<i8>(lex))),
                    "nonNegativeInteger" => Some(Self::Number(
                        SparqlNumber::parse_integer(lex).check(|n| !n.is_negative()),
                    )),
                    "unsignedLong" => Some(Self::Number(SparqlNumber::parse::<u64>(lex))),
                    "unsignedInt" => Some(Self::Number(SparqlNumber::parse::<u32>(lex))),
                    "unsignedShort" => Some(Self::Number(SparqlNumber::parse::<u16>(lex))),
                    "unsignedByte" => Some(Self::Number(SparqlNumber::parse::<u8>(lex))),
                    "positiveInteger" => Some(Self::Number(
                        SparqlNumber::parse_integer(lex).check(|n| n.is_positive()),
                    )),
                    _ => None,
                }
            }
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            SparqlValue::Number(n) => Some(n.is_truthy()),
            SparqlValue::String(val, _) => Some(!val.is_empty()),
            SparqlValue::Boolean(opt) => opt.or(Some(false)),
            SparqlValue::DateTime(_) => None,
        }
    }

    pub fn sparql_eq(&self, other: &Self) -> Option<bool> {
        use SparqlValue::*;
        match (self, other) {
            (Number(n1), Number(n2)) => Some(n1 == n2),
            (String(s1, None), String(s2, None)) => Some(s1 == s2),
            (String(s1, Some(t1)), String(s2, Some(t2))) => Some(t1 == t2 && s1 == s2),
            (Boolean(b1), Boolean(b2)) => Some(b1 == b2),
            (DateTime(d1), DateTime(d2)) => d1.partial_cmp(d2).map(|o| o == Ordering::Equal),
            _ => None,
        }
    }
}

impl From<bool> for SparqlValue {
    fn from(value: bool) -> Self {
        SparqlValue::Boolean(Some(value))
    }
}

impl From<SparqlNumber> for SparqlValue {
    fn from(value: SparqlNumber) -> Self {
        SparqlValue::Number(value)
    }
}

impl PartialEq for SparqlValue {
    fn eq(&self, other: &Self) -> bool {
        self.sparql_eq(other).unwrap_or(false)
    }
}

impl PartialOrd for SparqlValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use SparqlValue::*;
        match (self, other) {
            (Number(n1), Number(n2)) => n1.partial_cmp(&n2),
            (String(s1, None), String(s2, None)) => Some(s1.cmp(s2)),
            (String(s1, Some(t1)), String(s2, Some(t2))) => {
                Some(t1.cmp(t2).then_with(|| s1.cmp(s2)))
            }
            (Boolean(Some(b1)), Boolean(Some(b2))) => Some(b1.cmp(b2)),
            (DateTime(Some(d1)), DateTime(Some(d2))) => d1.partial_cmp(d2),
            _ => None,
        }
    }
}

const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
