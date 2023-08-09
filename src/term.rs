use std::{fmt, sync::OnceLock};

use sophia::{
    api::{
        term::{Term, TryFromTerm},
        MownStr,
    },
    term::ArcTerm,
};

use crate::number::SparqlNumber;

#[derive(Clone, Debug)]
pub struct ResultTerm {
    inner: ArcTerm,
    value: OnceLock<Option<SparqlNumber>>,
}

impl Term for ResultTerm {
    type BorrowTerm<'x> = &'x ArcTerm where Self: 'x;

    fn kind(&self) -> sophia::api::term::TermKind {
        self.inner.kind()
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        &self.inner
    }

    fn is_iri(&self) -> bool {
        self.inner.is_iri()
    }

    fn is_blank_node(&self) -> bool {
        self.inner.is_blank_node()
    }

    fn is_literal(&self) -> bool {
        self.inner.is_literal()
    }

    fn is_variable(&self) -> bool {
        self.inner.is_variable()
    }

    fn is_atom(&self) -> bool {
        self.inner.is_atom()
    }

    fn is_triple(&self) -> bool {
        self.inner.is_triple()
    }

    fn iri(&self) -> Option<sophia::api::term::IriRef<MownStr>> {
        self.inner.iri()
    }

    fn bnode_id(&self) -> Option<sophia::api::term::BnodeId<MownStr>> {
        self.inner.bnode_id()
    }

    fn lexical_form(&self) -> Option<MownStr> {
        self.inner.lexical_form()
    }

    fn datatype(&self) -> Option<sophia::api::term::IriRef<MownStr>> {
        self.inner.datatype()
    }

    fn language_tag(&self) -> Option<sophia::api::term::LanguageTag<MownStr>> {
        self.inner.language_tag()
    }

    fn variable(&self) -> Option<sophia::api::term::VarName<MownStr>> {
        self.inner.variable()
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.inner.triple()
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        self.inner.to_triple().map(|tr| tr.map(From::from))
    }
}

impl ResultTerm {
    fn value(&self) -> Option<&SparqlNumber> {
        self.value
            .get_or_init(|| SparqlNumber::try_from_term(&self.inner).ok())
            .as_ref()
    }
}

impl From<ArcTerm> for ResultTerm {
    fn from(inner: ArcTerm) -> Self {
        Self {
            inner,
            value: OnceLock::new(),
        }
    }
}

impl From<[ResultTerm; 3]> for ResultTerm {
    fn from([s, p, o]: [ResultTerm; 3]) -> Self {
        Self {
            inner: ArcTerm::Triple(Box::new([s.inner, p.inner, o.inner])),
            value: OnceLock::new(),
        }
    }
}

impl<T: Term> PartialEq<T> for ResultTerm {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl Eq for ResultTerm {}

impl std::hash::Hash for ResultTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

impl<T: Term> PartialOrd<T> for ResultTerm {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl Ord for ResultTerm {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, other.borrow_term())
    }
}

impl fmt::Display for ResultTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf: Vec<u8> = vec![];
        sophia::turtle::serializer::nt::write_term(&mut buf, self.borrow_term())
            .map_err(|_| fmt::Error)?;
        let txt = String::from_utf8(buf).map_err(|_| fmt::Error)?;
        txt.fmt(f)
    }
}
