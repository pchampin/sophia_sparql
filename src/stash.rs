//! Extend [ArcStrStash] with useful methods

use std::sync::Arc;

use bigdecimal::BigDecimal;
use sophia::{
    api::{
        ns::xsd,
        term::{Term, VarName},
    },
    term::{ArcStrStash, ArcTerm, GenericLiteral},
};

use crate::{number::SparqlNumber, value::SparqlValue, ResultTerm};

pub trait ArcStrStashExt {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm;
    fn value_to_term(&mut self, n: SparqlValue) -> ResultTerm;
    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>>;
}

impl ArcStrStashExt for ArcStrStash {
    fn copy_result_term<T: Term>(&mut self, t: T) -> ResultTerm {
        ResultTerm::from(self.copy_term(t))
    }

    fn copy_variable(&mut self, v: &spargebra::term::Variable) -> VarName<Arc<str>> {
        VarName::new_unchecked(self.copy_str(v.as_str()))
    }

    fn value_to_term(&mut self, value: SparqlValue) -> ResultTerm {
        value_to_term(value, |txt| self.copy_str(txt))
    }
}

pub fn value_to_term<F: FnMut(&str) -> Arc<str>>(value: SparqlValue, factory: F) -> ResultTerm {
    let inner = value_ref_to_arcterm(&value, factory);
    ResultTerm::from_parts(inner, Some(value))
}

pub fn value_ref_to_arcterm<F: FnMut(&str) -> Arc<str>>(
    value: &SparqlValue,
    mut factory: F,
) -> ArcTerm {
    let (lex, dt) = match value {
        SparqlValue::Number(SparqlNumber::NativeInt(i)) => (factory(&i.to_string()), xsd::integer),
        SparqlValue::Number(SparqlNumber::BigInt(i)) => (factory(&i.to_string()), xsd::integer),
        SparqlValue::Number(SparqlNumber::Decimal(d)) => (factory(&dec2string(d)), xsd::decimal),
        SparqlValue::Number(SparqlNumber::Float(f)) => (factory(&format!("{f:e}")), xsd::float),
        SparqlValue::Number(SparqlNumber::Double(d)) => (factory(&format!("{d:e}")), xsd::double),
        SparqlValue::Number(SparqlNumber::IllFormed) => (factory("ill-formed"), xsd::integer),
        SparqlValue::Boolean(None) => (factory("ill-formed"), xsd::boolean),
        SparqlValue::Boolean(Some(b)) => (factory(if *b { "true" } else { "false" }), xsd::boolean),
        SparqlValue::DateTime(None) => (factory("ill-formed"), xsd::dateTime),
        SparqlValue::DateTime(Some(d)) => (factory(&d.to_string()), xsd::dateTime),
        SparqlValue::String(lex, None) => (lex.clone(), xsd::string),
        SparqlValue::String(lex, Some(tag)) => {
            return ArcTerm::Literal(GenericLiteral::LanguageString(lex.clone(), tag.clone()));
        }
    };
    let dt = dt.iri().unwrap().as_ref().map_unchecked(factory);
    ArcTerm::Literal(GenericLiteral::Typed(lex, dt))
}

pub fn dec2string(d: &BigDecimal) -> String {
    let d = d.normalized();
    if d.fractional_digit_count() <= 0 {
        format!("{}.0", d.with_scale(0))
    } else {
        d.to_string()
    }
}
