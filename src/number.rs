use std::string::ToString;

use sophia::api::{
    ns::xsd,
    term::{Term, TermKind, TryFromTerm},
    MownStr,
};
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum SparqlNumber {
    NativeInt(isize),
    BigInt(String),  // TODO find some bigint library
    Decimal(String), // TODO find some decimal library
    Double(f64),
}

impl Term for SparqlNumber {
    type BorrowTerm<'x> = &'x SparqlNumber where Self: 'x;

    fn kind(&self) -> sophia::api::term::TermKind {
        TermKind::Literal
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn lexical_form(&self) -> Option<MownStr> {
        match self {
            SparqlNumber::NativeInt(value) => Some(value.to_string().into()),
            SparqlNumber::BigInt(value) => Some(value.to_string().into()),
            SparqlNumber::Decimal(value) => Some(value.to_string().into()),
            SparqlNumber::Double(value) => Some(value.to_string().into()),
        }
    }

    fn datatype(&self) -> Option<sophia::api::term::IriRef<MownStr>> {
        match self {
            SparqlNumber::NativeInt(_) => xsd::integer.iri(),
            SparqlNumber::BigInt(_) => xsd::integer.iri(),
            SparqlNumber::Decimal(_) => xsd::decimal.iri(),
            SparqlNumber::Double(_) => xsd::double.iri(),
        }
    }

    fn language_tag(&self) -> Option<sophia::api::term::LanguageTag<MownStr>> {
        None
    }
}

impl TryFromTerm for SparqlNumber {
    type Error = SparqlNumberError;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug, Error)]
pub enum SparqlNumberError {
    #[error("Misc: {0}")]
    Misc(String),
}
