//! Extend [ArcStrStash] with useful methods

use std::sync::Arc;

use datetime::ISO;
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

pub fn value_to_term<F: FnMut(&str) -> Arc<str>>(value: SparqlValue, mut factory: F) -> ResultTerm {
    let (lex, dt) = match &value {
        SparqlValue::Number(SparqlNumber::NativeInt(i)) => (factory(&i.to_string()), xsd::integer),
        SparqlValue::Number(SparqlNumber::BigInt(i)) => (factory(&i.to_string()), xsd::integer),
        SparqlValue::Number(SparqlNumber::Decimal(d)) => (factory(&d.to_string()), xsd::decimal),
        SparqlValue::Number(SparqlNumber::Float(f)) => (factory(&f.to_string()), xsd::float),
        SparqlValue::Number(SparqlNumber::Double(d)) => (factory(&d.to_string()), xsd::double),
        SparqlValue::Number(SparqlNumber::IllFormed) => (factory("ill-formed"), xsd::integer),
        SparqlValue::String(lex, None) => (lex.clone(), xsd::string),
        SparqlValue::String(lex, Some(tag)) => {
            return ResultTerm::from_parts(
                ArcTerm::Literal(GenericLiteral::LanguageString(lex.clone(), tag.clone())),
                Some(value),
            )
        }
        SparqlValue::Boolean(None) => (factory("ill-formed"), xsd::boolean),
        SparqlValue::Boolean(Some(b)) => (factory(if *b { "true" } else { "false" }), xsd::boolean),
        SparqlValue::DateTime(None) => (factory("ill-formed"), xsd::dateTime),
        SparqlValue::DateTime(Some(d)) => (factory(&d.iso().to_string()), xsd::dateTime),
    };
    let dt = dt.iri().unwrap().as_ref().map_unchecked(factory);
    let inner = ArcTerm::Literal(GenericLiteral::Typed(lex, dt));
    ResultTerm::from_parts(inner, Some(value))
}
