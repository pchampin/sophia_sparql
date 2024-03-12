use std::sync::Arc;

use bigdecimal::BigDecimal;
use datetime::OffsetDateTime;
use sophia::{
    api::term::LanguageTag,
    term::{ArcTerm, GenericLiteral},
};

use crate::number::SparqlNumber;

#[derive(Clone, Debug)]
pub enum SparqlValue {
    Number(SparqlNumber),
    String(Arc<str>, Option<LanguageTag<Arc<str>>>),
    Boolean(Option<bool>),
    DateTime(Option<OffsetDateTime>),
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

const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
