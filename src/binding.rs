use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

use sophia::api::prelude::*;
use sophia::api::sparql::SparqlBindings;
use sophia::api::term::SimpleTerm;
use sophia::api::term::VarName;
use sophia::term::ArcStrStash;
use spargebra::term::NamedNodePattern;
use spargebra::term::TermPattern;
use spargebra::term::TriplePattern;

use crate::matcher::AnyPattern;
use crate::stash::ArcStrStashExt;
use crate::term::ResultTerm;
use crate::SparqlWrapper;
use crate::SparqlWrapperError;

pub struct Bindings<'a, D: Dataset> {
    pub(crate) variables: Vec<VarName<Arc<str>>>,
    pub(crate) iter: Box<dyn Iterator<Item = Result<Binding, SparqlWrapperError<D::Error>>> + 'a>,
}

impl<'a, D: Dataset> Bindings<'a, D> {
    pub fn variables(&self) -> Vec<&str> {
        self.variables.iter().map(VarName::as_str).collect()
    }
}

impl<'a, D: Dataset> SparqlBindings<SparqlWrapper<'a, D>> for Bindings<'a, D> {
    fn variables(&self) -> Vec<&str> {
        Bindings::variables(self)
    }
}

impl<'a, D: Dataset> IntoIterator for Bindings<'a, D> {
    type Item = Result<Vec<Option<ResultTerm>>, SparqlWrapperError<D::Error>>;

    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter.map(move |rb| {
            rb.map(|b| {
                self.variables
                    .iter()
                    .map(|v| b.v.get(v.as_str()).map(Clone::clone))
                    .collect::<Vec<_>>()
            })
        }))
    }
}

#[derive(Clone, Debug, Default)]
pub struct Binding {
    /// Binding of variables
    pub v: BindingMap,
    /// Binding of blank nodes
    pub b: BindingMap,
}

pub type BindingMap = HashMap<Arc<str>, ResultTerm>;

pub fn populate_variables(
    patterns: &[TriplePattern],
    stash: &mut ArcStrStash,
) -> Vec<VarName<Arc<str>>> {
    let mut variable_set = HashSet::new();

    use spargebra::term::Variable;
    fn collect_variables<'a, T>(pattern: &'a T, set: &mut HashSet<&'a Variable>)
    where
        AnyPattern<'a>: From<&'a T>,
    {
        set.extend(AnyPattern::from(pattern).atoms().filter_map(|i| match i {
            AnyPattern::Term(TermPattern::Variable(v)) => Some(v),
            AnyPattern::Named(NamedNodePattern::Variable(v)) => Some(v),
            _ => None,
        }))
    }

    for tp in patterns {
        collect_variables(&tp.subject, &mut variable_set);
        collect_variables(&tp.predicate, &mut variable_set);
        collect_variables(&tp.object, &mut variable_set);
    }
    variable_set
        .iter()
        .map(|v| stash.copy_variable(v))
        .collect()
}

pub(crate) fn populate_bindings<T: Term>(
    pattern: &TriplePattern,
    result: &[T; 3],
    b: &mut Binding,
    stash: &mut ArcStrStash,
) -> Result<(), ()> {
    populate_bindings_term(AnyPattern::Term(&pattern.subject), &result[0], b, stash)?;
    populate_bindings_term(AnyPattern::Named(&pattern.predicate), &result[1], b, stash)?;
    populate_bindings_term(AnyPattern::Term(&pattern.object), &result[2], b, stash)?;
    Ok(())
}

fn populate_bindings_term<T: Term>(
    pattern: AnyPattern,
    result: &T,
    b: &mut Binding,
    stash: &mut ArcStrStash,
) -> Result<(), ()> {
    let st = pattern.as_simple();
    match st {
        SimpleTerm::BlankNode(bnid) => {
            if let Some(already_bound) = b.b.get(bnid.as_str()) {
                if !Term::eq(already_bound, result.borrow_term()) {
                    return Err(());
                }
            } else {
                b.b.insert(
                    stash.copy_str(bnid),
                    stash.copy_result_term(result.borrow_term()),
                );
            }
        }
        SimpleTerm::Variable(var) => {
            if let Some(already_bound) = b.v.get(var.as_str()) {
                if !Term::eq(already_bound, result.borrow_term()) {
                    return Err(());
                }
            } else {
                b.v.insert(
                    stash.copy_str(var),
                    stash.copy_result_term(result.borrow_term()),
                );
            }
        }
        SimpleTerm::Triple(_) => {
            let AnyPattern::Term(TermPattern::Triple(triple_pattern)) = pattern else {
                unreachable!();
            };
            debug_assert!(result.is_triple());
            let result = result.triple().unwrap();
            populate_bindings(triple_pattern, &result, b, stash)?;
        }
        _ => {
            debug_assert!(Term::eq(&pattern, result.borrow_term()));
        }
    }
    Ok(())
}
