//! An ArcTerm version of spargebra::Expression
use sophia::{
    api::{
        dataset::Dataset,
        term::{LanguageTag, Term, VarName},
    },
    iri::IriRef,
    term::{ArcStrStash, ArcTerm, GenericLiteral},
};
use spargebra::algebra::{Expression, Function, GraphPattern};

use std::{
    cmp::Ordering,
    sync::{Arc, LazyLock},
};

use crate::{
    binding::Binding,
    exec::{ExecConfig, ExecState},
    function::call_function,
    number::SparqlNumber,
    stash::{value_ref_to_arcterm, value_to_term, ArcStrStashExt},
    value::SparqlValue,
    ResultTerm,
};

/// An [expression](https://www.w3.org/TR/sparql11-query/#expressions).
#[derive(Debug, Clone)]
pub(crate) enum ArcExpression {
    NamedNode(IriRef<Arc<str>>),
    Literal(GenericLiteral<Arc<str>>),
    Variable(VarName<Arc<str>>),
    /// [Logical-or](https://www.w3.org/TR/sparql11-query/#func-logical-or).
    Or(Box<Self>, Box<Self>),
    /// [Logical-and](https://www.w3.org/TR/sparql11-query/#func-logical-and).
    And(Box<Self>, Box<Self>),
    /// [RDFterm-equal](https://www.w3.org/TR/sparql11-query/#func-RDFterm-equal) and all the XSD equalities.
    Equal(Box<Self>, Box<Self>),
    /// [sameTerm](https://www.w3.org/TR/sparql11-query/#func-sameTerm).
    SameTerm(Box<Self>, Box<Self>),
    /// [op:numeric-greater-than](https://www.w3.org/TR/xpath-functions-31/#func-numeric-greater-than) and other XSD greater than operators.
    Greater(Box<Self>, Box<Self>),
    GreaterOrEqual(Box<Self>, Box<Self>),
    /// [op:numeric-less-than](https://www.w3.org/TR/xpath-functions-31/#func-numeric-less-than) and other XSD greater than operators.
    Less(Box<Self>, Box<Self>),
    LessOrEqual(Box<Self>, Box<Self>),
    /// [IN](https://www.w3.org/TR/sparql11-query/#func-in)
    In(Box<Self>, Vec<Self>),
    /// [op:numeric-add](https://www.w3.org/TR/xpath-functions-31/#func-numeric-add) and other XSD additions.
    Add(Box<Self>, Box<Self>),
    /// [op:numeric-subtract](https://www.w3.org/TR/xpath-functions-31/#func-numeric-subtract) and other XSD subtractions.
    Subtract(Box<Self>, Box<Self>),
    /// [op:numeric-multiply](https://www.w3.org/TR/xpath-functions-31/#func-numeric-multiply) and other XSD multiplications.
    Multiply(Box<Self>, Box<Self>),
    /// [op:numeric-divide](https://www.w3.org/TR/xpath-functions-31/#func-numeric-divide) and other XSD divides.
    Divide(Box<Self>, Box<Self>),
    /// [op:numeric-unary-plus](https://www.w3.org/TR/xpath-functions-31/#func-numeric-unary-plus) and other XSD unary plus.
    UnaryPlus(Box<Self>),
    /// [op:numeric-unary-minus](https://www.w3.org/TR/xpath-functions-31/#func-numeric-unary-minus) and other XSD unary minus.
    UnaryMinus(Box<Self>),
    /// [fn:not](https://www.w3.org/TR/xpath-functions-31/#func-not).
    Not(Box<Self>),
    /// [EXISTS](https://www.w3.org/TR/sparql11-query/#func-filter-exists).
    Exists(Box<GraphPattern>),
    /// [BOUND](https://www.w3.org/TR/sparql11-query/#func-bound).
    Bound(VarName<Arc<str>>),
    /// [IF](https://www.w3.org/TR/sparql11-query/#func-if).
    If(Box<Self>, Box<Self>, Box<Self>),
    /// [COALESCE](https://www.w3.org/TR/sparql11-query/#func-coalesce).
    Coalesce(Vec<Self>),
    /// A regular function call.
    FunctionCall(Function, Vec<Self>),
}

impl ArcExpression {
    pub(crate) fn from_expr(expr: &Expression, stash: &mut ArcStrStash) -> Self {
        use ArcExpression::*;
        match expr {
            Expression::NamedNode(iri) => {
                let iri = stash.copy_str(iri.as_str());
                NamedNode(IriRef::new_unchecked(iri))
            }
            Expression::Literal(lit) => Literal({
                let lex = stash.copy_str(lit.value());
                if let Some(tag) = lit.language() {
                    GenericLiteral::LanguageString(
                        lex,
                        stash.copy_language_tag(LanguageTag::new_unchecked(tag)),
                    )
                } else {
                    GenericLiteral::Typed(
                        lex,
                        stash.copy_iri(IriRef::new_unchecked(lit.datatype().as_str())),
                    )
                }
            }),
            Expression::Variable(v) => Variable(stash.copy_variable(v)),
            Expression::Or(lhs, rhs) => Or(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::And(lhs, rhs) => And(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Equal(lhs, rhs) => Equal(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::SameTerm(lhs, rhs) => SameTerm(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Greater(lhs, rhs) => Greater(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::GreaterOrEqual(lhs, rhs) => GreaterOrEqual(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Less(lhs, rhs) => Less(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::LessOrEqual(lhs, rhs) => LessOrEqual(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::In(lhs, rhs) => In(
                Box::new(Self::from_expr(lhs, stash)),
                rhs.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
            Expression::Add(lhs, rhs) => Add(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Subtract(lhs, rhs) => Subtract(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Multiply(lhs, rhs) => Multiply(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Divide(lhs, rhs) => Divide(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::UnaryPlus(e) => UnaryPlus(Box::new(Self::from_expr(e, stash))),
            Expression::UnaryMinus(e) => UnaryMinus(Box::new(Self::from_expr(e, stash))),
            Expression::Not(e) => Not(Box::new(Self::from_expr(e, stash))),
            Expression::Exists(bpg) => Exists(bpg.clone()),
            Expression::Bound(v) => Bound(stash.copy_variable(v)),
            Expression::If(i, t, e) => If(
                Box::new(Self::from_expr(i, stash)),
                Box::new(Self::from_expr(t, stash)),
                Box::new(Self::from_expr(e, stash)),
            ),
            Expression::Coalesce(es) => Coalesce(
                es.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
            Expression::FunctionCall(func, args) => FunctionCall(
                func.clone(),
                args.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
        }
    }

    // NB: `config` is a reference to an Arc.
    // * why not just an Arc: to avoid the case of cloning it at each step of recursion;
    // * why not just a reference: because the Exists variant needs an Arc to build an ExecState.
    pub fn eval<D>(
        &self,
        binding: &Binding,
        config: &Arc<ExecConfig<'_, D>>,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Option<EvalResult>
    where
        D: Dataset,
    {
        use ArcExpression::*;
        match self {
            NamedNode(iri) => Some(ResultTerm::from_parts(ArcTerm::Iri(iri.clone()), None).into()),
            Literal(lit) => Some(ResultTerm::from(ArcTerm::Literal(lit.clone())).into()),
            Variable(var) => binding.v.get(var.as_str()).cloned().map(EvalResult::from),
            Or(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.is_truthy();
                let rhs = rhs.eval(binding, config, graph_matcher)?.is_truthy();
                match (lhs, rhs) {
                    (None, None) => None,
                    (Some(a), Some(b)) => Some(a || b),
                    (Some(true), None) | (None, Some(true)) => Some(true),
                    (Some(false), None) | (None, Some(false)) => None,
                }
                .map(EvalResult::from)
            }
            And(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.is_truthy();
                let rhs = rhs.eval(binding, config, graph_matcher)?.is_truthy();
                match (lhs, rhs) {
                    (None, None) => None,
                    (Some(a), Some(b)) => Some(a && b),
                    (Some(true), None) | (None, Some(true)) => None,
                    (Some(false), None) | (None, Some(false)) => Some(false),
                }
                .map(EvalResult::from)
            }
            Equal(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_eq(&rhs).map(EvalResult::from)
            }
            SameTerm(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.into_term();
                let rhs = rhs.eval(binding, config, graph_matcher)?.into_term();
                Some(Term::eq(&lhs, rhs).into())
            }
            Greater(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_gt()))
            }
            GreaterOrEqual(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_ge()))
            }
            Less(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_lt()))
            }
            LessOrEqual(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_le()))
            }
            In(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                rhs.iter()
                    .map(|other| {
                        other
                            .eval(binding, config, graph_matcher)
                            .and_then(|other| lhs.sparql_eq(&other))
                    })
                    .find(|res| res != &Some(false))
                    .unwrap_or(Some(false))
                    .map(EvalResult::from)
                // I reproduce the behaviour of Jena:
                // the IN operator fails on the first error, even if a match exists further in the list.
                // Another reasonable behaviour would be to ignore errors, and return false if no element in rhs was equal to lhs.
                // TODO check what the spec says
            }
            Add(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number()? + rhs.as_number()?).map(|n| SparqlValue::from(n).into())
            }
            Subtract(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number()? - rhs.as_number()?).map(|n| SparqlValue::from(n).into())
            }
            Multiply(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number()? * rhs.as_number()?).map(|n| SparqlValue::from(n).into())
            }
            Divide(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number()? / rhs.as_number()?).map(|n| SparqlValue::from(n).into())
            }
            UnaryPlus(e) => {
                let e = e.eval(binding, config, graph_matcher)?;
                e.as_number().map(|n| SparqlValue::from(n.clone()).into())
            }
            UnaryMinus(e) => {
                let e = e.eval(binding, config, graph_matcher)?;
                (-e.as_number()?).map(|n| SparqlValue::from(n).into())
            }
            Not(e) => {
                let e = e.eval(binding, config, graph_matcher)?.is_truthy()?;
                Some((!e).into())
            }
            Exists(graph_pattern) => {
                let mut exec_state = ExecState::from(Arc::clone(config));
                let res = exec_state.select(graph_pattern, graph_matcher, Some(binding));
                let exists = match res {
                    Ok(mut bindings) => bindings.iter.next().is_some(),
                    Err(_) => false,
                };
                Some(exists.into())
            }
            Bound(varname) => Some(binding.v.contains_key(varname.as_str()).into()),
            If(c, t, e) => {
                if c.eval(binding, config, graph_matcher)?
                    .is_truthy()
                    .unwrap_or(false)
                {
                    t.eval(binding, config, graph_matcher)
                } else {
                    e.eval(binding, config, graph_matcher)
                }
            }
            Coalesce(exprs) => exprs
                .iter()
                .find_map(|e| e.eval(binding, config, graph_matcher)),
            FunctionCall(function, arguments) => {
                let evaluated: Vec<EvalResult> = arguments
                    .iter()
                    .map_while(|e| e.eval(binding, config, graph_matcher))
                    .collect();
                if evaluated.len() == arguments.len() {
                    call_function(function, evaluated)
                } else {
                    None
                }
            }
        }
    }
}

//

#[derive(Clone, Debug)]
pub enum EvalResult {
    Term(ResultTerm),
    Value(SparqlValue),
}

impl EvalResult {
    pub fn as_value(&self) -> Option<&SparqlValue> {
        match self {
            EvalResult::Term(t) => t.value(),
            EvalResult::Value(v) => Some(v),
        }
    }

    pub fn as_number(&self) -> Option<&SparqlNumber> {
        match self.as_value() {
            Some(SparqlValue::Number(n)) => Some(n),
            _ => None,
        }
    }

    pub fn as_term(&self) -> ArcTerm {
        match self {
            EvalResult::Term(t) => t.borrow_term().clone(),
            EvalResult::Value(v) => value_ref_to_arcterm(v, |txt| Arc::from(txt)),
        }
    }

    pub fn into_term(self) -> ResultTerm {
        match self {
            EvalResult::Term(t) => t,
            EvalResult::Value(v) => value_to_term(v, |txt| Arc::from(txt)),
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        self.as_value().and_then(SparqlValue::is_truthy)
    }

    pub fn sparql_eq(&self, other: &Self) -> Option<bool> {
        if let (Some(s), Some(o)) = (self.as_value(), other.as_value()) {
            s.sparql_eq(o)
        } else {
            let s = self.as_term();
            let o = other.as_term();
            if Term::eq(&s, &o) {
                Some(true)
            } else if s.is_literal() && o.is_literal() {
                None // distinct unrecognized literals can not be compared
            } else {
                Some(false)
            }
        }
    }

    pub fn sparql_cmp(&self, other: &Self) -> Option<Ordering> {
        if let (Some(s), Some(o)) = (self.as_value(), other.as_value()) {
            s.partial_cmp(o)
        } else {
            let s = self.as_term();
            let o = other.as_term();
            if s.is_literal() && o.is_literal() && Term::eq(&s, &o) {
                Some(Ordering::Equal)
            } else {
                None // distinct unrecognized literals can not be compared
            }
        }
    }
}

impl From<ResultTerm> for EvalResult {
    fn from(value: ResultTerm) -> Self {
        EvalResult::Term(value)
    }
}

impl From<SparqlValue> for EvalResult {
    fn from(value: SparqlValue) -> Self {
        EvalResult::Value(value)
    }
}

impl From<bool> for EvalResult {
    fn from(value: bool) -> Self {
        EvalResult::Value(value.into())
    }
}

impl From<Arc<str>> for EvalResult {
    fn from(value: Arc<str>) -> Self {
        EvalResult::Term(ArcTerm::from((value, XSD_STRING.clone())).into())
    }
}

pub(crate) static XSD_STRING: LazyLock<IriRef<Arc<str>>> =
    LazyLock::new(|| IriRef::new_unchecked(Arc::from("http://www.w3.org/2001/XMLSchema#string")));
