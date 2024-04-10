use sophia::api::prelude::*;
use sophia::term::ArcTerm;
use spargebra::term::TriplePattern;

use crate::binding::{populate_bindings, Binding};
use crate::exec::ExecState;
use crate::matcher::SparqlMatcher;
use crate::SparqlWrapperError;

pub fn make_iterator<D: Dataset>(
    state: &mut ExecState<D>,
    patterns: &[TriplePattern],
    graph_matcher: &[Option<ArcTerm>],
    binding: Option<&Binding>,
) -> impl Iterator<Item = Result<Binding, SparqlWrapperError<D::Error>>> {
    // TODO one day:
    // test the following "greedy" optimization :
    // 1. first search all ground triple patterns (no var/bnode)
    //    (rationale: they have 0 or 1 result, no need to continue if one of them fail)
    // 2. then try the triple pattern with the most distinct variables
    //    (rationale: bind as much variables/bnodes as possible,
    //     so that further triples are more constrained)
    // 3. then try all triples that contain only variables bound above
    //    (rationale: same as 1. -- those triples are now ground
    //     in the current context)
    // 4. if there are any triple patterns left, restart from 2
    //
    // NB: this can be achieved by simply sorting `patterns`
    // in the corresponding order (by simply looking at the vars/bnodes in the patterns),
    // then simply calling self.bgp_rec as below
    let mut bindings = vec![];
    let b = binding.cloned().unwrap_or_default();
    if let Err(e) = bgp_rec(state, patterns, &mut bindings, b, graph_matcher) {
        bindings.push(Err(e));
    }
    bindings.into_iter()
}

fn bgp_rec<D: Dataset>(
    state: &mut ExecState<D>,
    patterns: &[TriplePattern],
    bs: &mut Vec<Result<Binding, SparqlWrapperError<D::Error>>>,
    mut b: Binding,
    graph_matcher: &[Option<ArcTerm>],
) -> Result<(), SparqlWrapperError<D::Error>> {
    let [first, remaining @ ..] = &patterns else {
        // empty BGP, always succeeds
        bs.push(Ok(b));
        return Ok(());
    };
    let [sm, pm, om] = SparqlMatcher::build3(first, &b, state.stash_mut());
    let all_bound = sm.is_bound() && pm.is_bound() && om.is_bound();
    let matches: Vec<_> = state
        .config()
        .dataset
        .quads_matching(sm, pm, om, graph_matcher)
        .map(|res| res.map(Quad::into_triple))
        .collect::<Result<Vec<_>, _>>()
        .map_err(SparqlWrapperError::Dataset)?;
    let [first_matches @ .., last_match] = &matches[..] else {
        // no matches for this triple pattern, abort BGP matching
        return Ok(());
    };
    if all_bound {
        // triple-pattern has no unbound variable, continue with current bindings
        return bgp_rec(state, remaining, bs, b, graph_matcher);
    }
    for m in first_matches {
        let mut b = b.clone();
        if populate_bindings(first, m, &mut b, state.stash_mut()).is_ok() {
            bgp_rec(state, remaining, bs, b, graph_matcher)?;
        }
    }
    if populate_bindings(first, last_match, &mut b, state.stash_mut()).is_ok() {
        bgp_rec(state, remaining, bs, b, graph_matcher)?;
    }
    Ok(())
}

/*
pub struct BgpIterator<D> {
    _phantom: PhantomData<D>,
}

impl<D> Iterator for BgpIterator<D> {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
*/
