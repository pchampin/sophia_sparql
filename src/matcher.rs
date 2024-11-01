#![allow(clippy::module_name_repetitions)]

use std::sync::Arc;

use sophia::{
    api::{
        term::{
            matcher::TermMatcher, BnodeId, IriRef, LanguageTag, SimpleTerm, Term, TermKind, VarName,
        },
        MownStr,
    },
    term::ArcStrStash,
};
use spargebra::term::{NamedNodePattern, TermPattern, TriplePattern};

use crate::stash::ArcStrStashExt;
use crate::{binding::Binding, term::ResultTerm};

mod _any_pattern;
pub(crate) use _any_pattern::*;
mod _ox2so;
pub(crate) use _ox2so::*;

pub enum SparqlMatcher {
    Var(Arc<str>),
    Bnode(Arc<str>),
    // NB: this variant is for non-ground triples only;
    // for ground triples, the `Bound` variant should be used instead
    Triple(Box<[SparqlMatcher; 3]>),
    Bound(ResultTerm),
}

impl SparqlMatcher {
    pub fn build(pattern: AnyPattern, bindings: &Binding, stash: &mut ArcStrStash) -> Self {
        use SparqlMatcher::*;
        match pattern.as_simple() {
            SimpleTerm::BlankNode(bnid) => {
                if let Some(b) = bindings.b.get(bnid.as_str()) {
                    Bound(b.clone())
                } else {
                    Bnode(stash.copy_str(bnid))
                }
            }
            SimpleTerm::Triple(_) => {
                // the following is safe because we know we have a triple
                let tr = unsafe { pattern.to_triple().unwrap_unchecked() };
                match tr.map(|t| SparqlMatcher::build(t, bindings, stash)) {
                    [Bound(s), Bound(p), Bound(o)] => Bound([s, p, o].into()),
                    spo => Triple(Box::new(spo)),
                }
            }
            SimpleTerm::Variable(vn) => {
                if let Some(b) = bindings.v.get(vn.as_str()) {
                    Bound(b.clone())
                } else {
                    Var(stash.copy_str(vn))
                }
            }
            _ => Bound(stash.copy_result_term(pattern)),
        }
    }

    pub fn is_bound(&self) -> bool {
        matches!(self, SparqlMatcher::Bound(_))
    }

    pub fn build3(
        pattern: &TriplePattern,
        bindings: &Binding,
        stash: &mut ArcStrStash,
    ) -> [Self; 3] {
        [
            AnyPattern::from(&pattern.subject),
            AnyPattern::from(&pattern.predicate),
            AnyPattern::from(&pattern.object),
        ]
        .map(|p| Self::build(p, bindings, stash))
    }
}

impl TermMatcher for SparqlMatcher {
    type Term = ResultTerm;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        match self {
            SparqlMatcher::Bound(t) => Term::eq(t.borrow_term(), term.borrow_term()),
            SparqlMatcher::Triple(tr) => (
                tr[0].matcher_ref(),
                tr[1].matcher_ref(),
                tr[2].matcher_ref(),
            )
                .matches(term),
            _ => true,
        }
    }

    fn constant(&self) -> Option<&Self::Term> {
        match self {
            SparqlMatcher::Bound(t) => Some(t),
            _ => None,
        }
    }
}
