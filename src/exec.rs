use std::sync::Arc;

use sophia::api::prelude::*;
use sophia::term::ArcStrStash;
use sophia::term::ArcTerm;
use spargebra::algebra::GraphPattern;
use spargebra::algebra::QueryDataset;
use spargebra::term::TriplePattern;
use spargebra::term::Variable;

use crate::bgp;
use crate::binding::populate_variables;
use crate::binding::ArcStrStashExt;
use crate::binding::Bindings;
use crate::SparqlWrapperError;

#[derive(Clone, Debug)]
pub struct ExecState<'a, D> {
    stash: ArcStrStash,
    config: Arc<ExecConfig<'a, D>>,
}

#[derive(Clone, Debug)]
pub struct ExecConfig<'a, D> {
    pub dataset: &'a D,
    pub default_matcher: Vec<Option<ArcTerm>>,
    pub named_graphs: Vec<[Option<ArcTerm>; 1]>,
}

impl<'a, D: Dataset> ExecState<'a, D> {
    pub fn new(
        dataset: &'a D,
        query_dataset: &Option<QueryDataset>,
    ) -> Result<Self, SparqlWrapperError<D::Error>> {
        let mut stash = ArcStrStash::new();
        let default_matcher = match query_dataset {
            None => vec![None],
            Some(query_dataset) => query_dataset
                .default
                .iter()
                .map(|nn| {
                    Some(ArcTerm::Iri(IriRef::new_unchecked(
                        stash.copy_str(nn.as_str()),
                    )))
                })
                .collect(),
        };
        let named_graphs = match query_dataset.as_ref().and_then(|qd| qd.named.as_ref()) {
            None => dataset
                .graph_names()
                .map(|res| res.map(|t| [Some(stash.copy_term(t))]))
                .collect::<Result<Vec<_>, _>>()
                .map_err(SparqlWrapperError::Exec)?,
            Some(_) => todo!("FROM NAMED"),
        };
        let config = Arc::new(ExecConfig {
            dataset,
            default_matcher,
            named_graphs,
        });
        Ok(ExecState { stash, config })
    }

    pub fn config(&self) -> &ExecConfig<'a, D> {
        &self.config
    }

    pub fn config_cloned(&self) -> Arc<ExecConfig<'a, D>> {
        Arc::clone(&self.config)
    }

    pub fn stash_mut(&mut self) -> &mut ArcStrStash {
        &mut self.stash
    }

    pub fn select(
        &mut self,
        pattern: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        use GraphPattern::*;
        match pattern {
            Bgp { patterns } => self.bgp(patterns, graph_matcher),
            Path {
                subject,
                path,
                object,
            } => todo!("Path"),
            Join { left, right } => todo!("Join"),
            LeftJoin {
                left,
                right,
                expression,
            } => todo!("LeftJoin"),
            Filter { expr, inner } => todo!("Filter"),
            Union { left, right } => todo!("Union"),
            Graph { name, inner } => todo!("Graph"),
            Extend {
                inner,
                variable,
                expression,
            } => todo!("Extend"),
            Minus { left, right } => todo!("Minus"),
            Values {
                variables,
                bindings,
            } => todo!("Values"),
            OrderBy { inner, expression } => todo!("OrderBy"),
            Project { inner, variables } => self.project(graph_matcher, inner, variables),
            Distinct { inner } => todo!("Distinct"),
            Reduced { inner } => todo!("Reduced"),
            Slice {
                inner,
                start,
                length,
            } => self.slice(graph_matcher, inner, *start, *length),
            Group {
                inner,
                variables,
                aggregates,
            } => todo!("Group"),
            Service {
                name,
                inner,
                silent,
            } => todo!("Service"),
        }
    }

    fn bgp(
        &mut self,
        patterns: &[TriplePattern],
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let variables = populate_variables(patterns, &mut self.stash);
        let iter = Box::new(bgp::make_iterator(self, patterns, graph_matcher));
        Ok(Bindings { variables, iter })
    }

    fn project(
        &mut self,
        graph_matcher: &[Option<ArcTerm>],
        inner: &GraphPattern,
        variables: &[Variable],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let new_variables = variables
            .iter()
            .map(|v| self.stash.copy_variable(v))
            .collect();
        let mut bindings = self.select(inner, graph_matcher)?;
        bindings.variables = new_variables;
        Ok(bindings)
    }

    fn slice(
        &mut self,
        graph_matcher: &[Option<ArcTerm>],
        inner: &GraphPattern,
        start: usize,
        length: Option<usize>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let mut bindings = self.select(inner, graph_matcher)?;
        let skipped = bindings.iter.skip(start);
        bindings.iter = match length {
            Some(n) => Box::new(skipped.take(n)),
            None => Box::new(skipped),
        };
        Ok(bindings)
    }
}
