use std::{collections::HashSet, sync::Arc};

use crate::{
    expression::EvalResult,
    value::{SparqlNumber, SparqlValue},
    SparqlQuery, SparqlWrapper,
};

use sophia::{
    api::{
        ns::rdf,
        sparql::{Query, SparqlDataset},
        term::{BnodeId, IriRef, LanguageTag, Term},
    },
    inmem::dataset::LightDataset,
    term::GenericLiteral,
};
use test_case::test_case;

#[test_case("tag:x")]
fn str_iri(arg: &str) -> TestResult {
    let iri = IriRef::new_unchecked(Arc::<str>::from(arg));
    let got = super::str_iri(&iri);
    let exp = Some(EvalResult::from(Arc::<str>::from(arg)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
fn str_literal(lex: &str, lang: &str, dt: &str) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = super::str_literal(lit);
    let exp = Some(EvalResult::from(Arc::<str>::from(lex)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
fn lang(lex: &str, lang: &str, dt: &str) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = super::lang(lit);
    let exp = Some(EvalResult::from(Arc::<str>::from(lang)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", rdf::langString)]
fn datatype<T: ToString>(lex: &str, lang: &str, dt: T) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.to_string().into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = super::datatype(lit);
    let exp = Some(EvalResult::from(IriRef::new_unchecked(Arc::<str>::from(
        dt.to_string(),
    ))));
    assert!(eval_eq(dbg!(got), dbg!(exp)));
    Ok(())
}

#[test_case("tag:x", true)]
#[test_case("../a", true)]
#[test_case("a b", false)]
fn iri(arg: &str, exp: bool) -> TestResult {
    let arg = Arc::<str>::from(arg);
    let got = super::iri(&arg);
    let exp = true
        .then_some(arg)
        .and_then(|arg| IriRef::new(arg.clone()).ok())
        .map(EvalResult::from);
    assert!(eval_eq(dbg!(got), dbg!(exp)));
    Ok(())
}

#[test]
fn bnode1() -> TestResult {
    let Some(got) = super::bnode1(&Arc::from("foo")) else {
        panic!()
    };
    assert!(got.as_term().is_blank_node());
    Ok(())
}

#[test]
fn bnode0() -> TestResult {
    let mut set = HashSet::new();
    const N: usize = 5;
    for _ in 1..=N {
        let Some(EvalResult::Term(term)) = super::bnode0() else {
            panic!();
        };
        let bnid = term.bnode_id().unwrap().unwrap().to_string();
        set.insert(bnid);
    }
    assert_eq!(set.len(), N);
    Ok(())
}

#[test]
fn rand_all_diff() -> TestResult {
    let mut set = HashSet::new();
    const N: usize = 5;
    for _ in 1..=N {
        let Some(EvalResult::Value(SparqlValue::Number(SparqlNumber::Double(val)))) = super::rand()
        else {
            panic!();
        };
        assert!((0.0..1.0).contains(&val));
        set.insert(val.to_string());
    }
    assert_eq!(set.len(), N);
    Ok(())
}

// See https://www.w3.org/TR/sparql12-query/#func-concat
#[test_case(vec!["foo", "bar"], "foobar")]
#[test_case(vec!["foo@en", "bar@en"], "foobar@en")]
#[test_case(vec!["foo@en", "bar"], "foobar")]
#[test_case(vec!["foo", "bar@en"], "foobar")]
#[test_case(vec!["foo@en", "bar@es"], "foobar")]
#[test_case(vec!["abc"], "abc")]
#[test_case(vec!["abc@en"], "abc@en")]
#[test_case(vec![], "")]
// More arguments
#[test_case(vec!["a", "b", "c"], "abc")]
#[test_case(vec!["a", "b", "c", "d"], "abcd")]
fn concat(input: Vec<&str>, exp: &str) {
    fn txt2pair(txt: &str) -> (Arc<str>, Option<LanguageTag<Arc<str>>>) {
        let (lex, tag) = txt.split_once('@').unwrap_or((txt, ""));
        (
            Arc::from(lex),
            if tag.is_empty() {
                None
            } else {
                Some(LanguageTag::new_unchecked(Arc::from(tag)))
            },
        )
    }

    let input: Vec<_> = input.into_iter().map(txt2pair).collect();
    let args: Vec<_> = input.iter().map(|(lex, tag)| (lex, tag.as_ref())).collect();

    let exp = Some(EvalResult::from(txt2pair(exp)));
    assert!(eval_eq(super::concat(&args), exp));
}

#[test_case("en", "*", true)]
#[test_case("EN", "en", true)]
#[test_case("en-UK", "en", true)]
#[test_case("en-uk", "en-UK", true)]
#[test_case("en-US", "en-UK", false)]
#[test_case("en", "en-UK", false)]
#[test_case("es", "en", false)]
#[test_case("enx", "en", false)]
fn lang_matches(tag: &str, range: &str, exp: bool) -> TestResult {
    let (tag1, tag2) = eval_expr(&format!("\"{tag}\""))?;
    let tag2 = tag2.unwrap();
    let (range1, range2) = eval_expr(&format!("\"{range}\""))?;
    let range2 = range2.unwrap();
    let exp = Some(EvalResult::from(exp));
    assert!(eval_eq(super::lang_matches(&tag1, &range1), exp.clone()));
    assert!(eval_eq(super::lang_matches(&tag1, &range2), exp.clone()));
    assert!(eval_eq(super::lang_matches(&tag2, &range1), exp.clone()));
    assert!(eval_eq(super::lang_matches(&tag2, &range2), exp));
    Ok(())
}

#[test_case("<tag:x>"; "IRI")]
#[test_case("\"\""; "empty string")]
#[test_case("\"chat\"@en"; "language string")]
#[test_case("042"; "number")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>"; "triple")]
fn lang_matches_errs(arg: &str) -> TestResult {
    let en = EvalResult::from(Arc::<str>::from("en"));
    let (arg1, arg2) = eval_expr(arg)?;
    if let Some(arg2) = arg2 {
        assert!(dbg!(super::lang_matches(&arg2, &en)).is_none());
        assert!(dbg!(super::lang_matches(&en, &arg2)).is_none());
    }
    assert!(dbg!(super::lang_matches(&arg1, &en)).is_none());
    assert!(dbg!(super::lang_matches(&en, &arg1)).is_none());
    Ok(())
}

#[test]
fn lang_matches_for_bnode() {
    let en = EvalResult::from(Arc::<str>::from("en"));
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    assert!(dbg!(super::lang_matches(&en, &bnode)).is_none());
    assert!(dbg!(super::lang_matches(&bnode, &en)).is_none());
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
