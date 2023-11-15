use std::borrow::Borrow;
use std::marker::PhantomData;

use sophia::api::prelude::*;
use sophia::api::sparql::*;
use spargebra::Query as QueryAST;
use thiserror::Error;

use crate::{binding::Bindings, exec::ExecState, term::ResultTerm};

#[derive(Debug)]
/// Wrap any Sophia [`Dataset`] into a [`SparqlDataset`],
/// which can therefore be queried with SPARQL.
pub struct SparqlWrapper<'a, D: Dataset>(pub &'a D);

impl<'a, D: Dataset> SparqlDataset for SparqlWrapper<'a, D> {
    type BindingsTerm = ResultTerm;

    type BindingsResult = Bindings<'a, D>;

    type TriplesResult = Box<dyn Iterator<Item = Result<[Self::BindingsTerm; 3], D::Error>>>;

    type SparqlError = SparqlWrapperError<D::Error>;

    type Query = SparqlQuery<D>;

    fn query<Q>(&self, query: Q) -> Result<SparqlResult<Self>, Self::SparqlError>
    where
        Q: IntoQuery<Self::Query>,
    {
        let query = query.into_query()?;
        match &(query.borrow().algebra) {
            QueryAST::Select {
                dataset,
                pattern,
                base_iri,
            } => {
                let mut exec = ExecState::new(self.0, dataset)?;
                let cfg = exec.config_cloned();
                exec.select(pattern, &cfg.default_matcher)
                    .map(SparqlResult::Bindings)
            }
            QueryAST::Construct {
                template,
                dataset,
                pattern,
                base_iri,
            } => todo!("CONSTRUCT query"),
            QueryAST::Describe {
                dataset,
                pattern,
                base_iri,
            } => todo!("DESCRIBE query"),
            QueryAST::Ask {
                dataset,
                pattern,
                base_iri,
            } => todo!("ASK query"),
        }
    }
}

//

#[derive(Error)]
pub enum SparqlWrapperError<E> {
    #[error("Query parse error: {0}")]
    Parse(#[from] spargebra::ParseError),
    #[error("Query execution error: {0}")]
    Exec(E),
}

impl<E: std::error::Error> std::fmt::Debug for SparqlWrapperError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

//

#[derive(Clone, Debug)]
pub struct SparqlQuery<D> {
    algebra: spargebra::Query,
    _phantom: PhantomData<D>,
}

impl<D: Dataset> From<spargebra::Query> for SparqlQuery<D> {
    fn from(algebra: spargebra::Query) -> Self {
        SparqlQuery {
            algebra,
            _phantom: PhantomData,
        }
    }
}

impl<D: Dataset> sophia::api::sparql::Query for SparqlQuery<D> {
    type Error = SparqlWrapperError<D::Error>;

    fn parse(query_source: &str) -> Result<Self, Self::Error> {
        Ok(SparqlQuery::from(QueryAST::parse(query_source, None)?))
    }
}
