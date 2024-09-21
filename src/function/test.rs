use std::sync::Arc;

use crate::{expression::EvalResult, SparqlQuery, SparqlWrapper};

use sophia::{
    api::{
        sparql::{Query, SparqlDataset},
        term::{BnodeId, IriRef, Term},
    },
    inmem::dataset::LightDataset,
    term::ArcTerm,
};
use test_case::test_case;

#[test_case("<tag:x>", "\"tag:x\""; "str for IRI")]
#[test_case("\"42\"", "\"42\""; "str for string")]
#[test_case("\"chat\"@en", "\"chat\""; "str for language string")]
#[test_case("42", "\"42\""; "str for number")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", "\"<< <tag:s> <tag:p> <tag:o> >>\""; "str for triple")]
fn str(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::str(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::str(&arg1)), exp));
    Ok(())
}

#[test]
fn str_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    let exp = EvalResult::from(Arc::<str>::from("_:b"));
    assert_eq!(dbg!(super::str(&bnode)).unwrap().as_term(), exp.as_term());
}

#[test_case("<tag:x>", ""; "lang for IRI")]
#[test_case("\"42\"", "\"\""; "lang for string")]
#[test_case("\"chat\"@en", "\"en\""; "lang for language string")]
#[test_case("042", "\"\""; "lang for number")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "lang for triple")]
fn lang(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::lang(&dbg!(arg2))), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::lang(&arg1)), exp));
    Ok(())
}

#[test]
fn lang_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    let exp = EvalResult::from(Arc::<str>::from("_:b"));
    assert!(dbg!(super::lang(&bnode)).is_none());
}

#[test_case("<tag:x>", ""; "datatype for IRI")]
#[test_case("\"42\"", "xsd:string"; "datatype for string")]
#[test_case("\"chat\"@en", "rdf:langString"; "datatype for language string")]
#[test_case("042", "xsd:integer"; "datatype for integer")]
#[test_case("3.14", "xsd:decimal"; "datatype for decimal")]
#[test_case("3.14e0", "xsd:double"; "datatype for double")]
#[test_case("\"1\"^^xsd:float", "xsd:float"; "datatype for float")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "datatype for triple")]
fn datatype(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::datatype(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::datatype(&arg1)), exp));
    Ok(())
}

#[test]
fn datatype_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    let exp = EvalResult::from(Arc::<str>::from("_:b"));
    assert!(dbg!(super::datatype(&bnode)).is_none());
}

#[test_case("<tag:x>", "<tag:x>"; "iri for IRI")]
#[test_case("\"tag:y\"", "<tag:y>"; "iri for string")]
#[test_case("\"a b\"", ""; "iri for string that is not an IRI")]
#[test_case("\"chat\"@en", ""; "iri for language string")]
#[test_case("042", ""; "iri for integer")]
#[test_case("3.14", ""; "iri for decimal")]
#[test_case("3.14e0", ""; "iri for double")]
#[test_case("\"1\"^^xsd:float", ""; "iri for float")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "iri for triple")]
fn iri(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(super::iri(&arg2), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::iri(&arg1)), exp));
    Ok(())
}

#[test]
fn iri_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    assert!(dbg!(super::iri(&bnode)).is_none());
}

#[test]
fn iri_for_sting_that_is_relative_iri() {
    let bnode = EvalResult::from(Arc::<str>::from("a"));
    let exp = ArcTerm::from(IriRef::new_unchecked(Arc::<str>::from("a")));
    assert_eq!(dbg!(super::iri(&bnode)).unwrap().as_term(), exp);
}

// TODO test bnode, rand

#[test_case("<tag:x>", ""; "abs for IRI")]
#[test_case("\"42\"", ""; "abs for string")]
#[test_case("\"chat\"@en", ""; "abs for language string")]
#[test_case("042", "42"; "abs for positive integer")]
#[test_case("3.14", "3.14"; "abs for positive decimal")]
#[test_case("3.14e0", "3.14e0"; "abs for positive double")]
#[test_case("\"1\"^^xsd:float", "\"1e0\"^^xsd:float"; "abs for positive float")]
#[test_case("-042", "42"; "abs for netative integer")]
#[test_case("-3.14", "3.14"; "abs for netative decimal")]
#[test_case("-3.14e0", "3.14e0"; "abs for netative double")]
#[test_case("\"-1\"^^xsd:float", "\"1e0\"^^xsd:float"; "abs for netative float")]
#[test_case("1e0/0", "\"inf\"^^xsd:double"; "abs for positive INF")]
#[test_case("-1e0/0", "\"inf\"^^xsd:double"; "abs for negative INF")]
#[test_case("0e0/0", "\"NaN\"^^xsd:double"; "abs for NaN")]
#[test_case("\"a\"^^xsd:integer", ""; "abs for ill formed")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "abs for triple")]
fn abs(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::abs(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::abs(&arg1)), exp));
    Ok(())
}

#[test]
fn abs_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    let exp = EvalResult::from(Arc::<str>::from("_:b"));
    assert!(dbg!(super::abs(&bnode)).is_none());
}

#[test_case("<tag:x>", ""; "ceil for IRI")]
#[test_case("\"42\"", ""; "ceil for string")]
#[test_case("\"chat\"@en", ""; "ceil for language string")]
#[test_case("042", "42"; "ceil for integer")]
#[test_case("3.14", "4.0"; "ceil for decimal")]
#[test_case("3.14e0", "4e0"; "ceil for double")]
#[test_case("\"1.5\"^^xsd:float", "\"2e0\"^^xsd:float"; "ceil for float")]
#[test_case("\"a\"^^xsd:integer", ""; "ceil for ill formed")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "ceil for triple")]
fn ceil(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::ceil(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::ceil(&arg1)), exp));
    Ok(())
}

#[test]
fn ceil_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    let exp = EvalResult::from(Arc::<str>::from("_:b"));
    assert!(dbg!(super::ceil(&bnode)).is_none());
}

/// Evaluate the given SPARQL expression,
/// returning one or two versions:
/// one EvalResult::Term and one EValResult::Value if appropriate.
fn eval_expr(expr: &str) -> TestResult<(EvalResult, Option<EvalResult>)> {
    eprintln!("eval_expr: {expr}");
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(
        &format!("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}")
    )?;
    let bindings = dataset.query(&query)?.into_bindings();
    assert_eq!(bindings.variables().len(), 1);
    let mut first_binding = bindings.into_iter().next().unwrap()?;
    assert_eq!(first_binding.len(), 1);
    let result = first_binding.pop().unwrap().unwrap();
    let as_value = result.value().cloned().map(EvalResult::Value);
    Ok((result.into(), as_value))
}

fn eval_eq(e1: Option<EvalResult>, e2: Option<EvalResult>) -> bool {
    match (e1, e2) {
        (Some(e1), Some(e2)) => Term::eq(&e1.into_term(), e2.into_term()),
        (None, None) => true,
        _ => false,
    }
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;
