use std::{collections::HashSet, sync::Arc};

use crate::{
    expression::EvalResult,
    value::{SparqlNumber, SparqlValue},
    SparqlQuery, SparqlWrapper,
};

use sophia::{
    api::{
        sparql::{Query, SparqlDataset},
        term::{BnodeId, IriRef, LanguageTag, Term},
    },
    inmem::dataset::LightDataset,
    term::{ArcTerm, GenericLiteral},
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

#[test_case("<tag:x>", false; "bnode for IRI")]
#[test_case("\"42\"", true; "bnode for string")]
#[test_case("\"chat\"@en", false; "bnode for language string")]
#[test_case("042", false; "bnode for integer")]
#[test_case("3.14", false; "bnode for decimal")]
#[test_case("3.14e0", false; "bnode for double")]
#[test_case("\"1\"^^xsd:float", false; "bnode for float")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", false; "bnode for triple")]
fn bnode(arg: &str, exp: bool) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    if let Some(arg2) = arg2 {
        if let Some(er) = super::bnode(Some(&arg2)) {
            assert!(exp);
            assert!(er.as_term().is_blank_node());
        } else {
            assert!(!exp);
        }
    }
    if let Some(er) = super::bnode(Some(&arg1)) {
        assert!(exp);
        assert!(er.as_term().is_blank_node());
    } else {
        assert!(!exp);
    }
    Ok(())
}

#[test]
fn bnode_no_arg_all_diff() -> TestResult {
    let mut set = HashSet::new();
    const N: usize = 5;
    for _ in 1..=N {
        let Some(EvalResult::Term(term)) = super::bnode(None) else {
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
    assert!(dbg!(super::ceil(&bnode)).is_none());
}

#[test_case("<tag:x>", ""; "floor for IRI")]
#[test_case("\"42\"", ""; "floor for string")]
#[test_case("\"chat\"@en", ""; "floor for language string")]
#[test_case("042", "42"; "floor for integer")]
#[test_case("3.14", "3.0"; "floor for decimal")]
#[test_case("3.14e0", "3e0"; "floor for double")]
#[test_case("\"1.5\"^^xsd:float", "\"1e0\"^^xsd:float"; "floor for float")]
#[test_case("\"a\"^^xsd:integer", ""; "floor for ill formed")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "floor for triple")]
fn floor(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::floor(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::floor(&arg1)), exp));
    Ok(())
}

#[test]
fn floor_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    assert!(dbg!(super::floor(&bnode)).is_none());
}

#[test_case("<tag:x>", ""; "round for IRI")]
#[test_case("\"42\"", ""; "round for string")]
#[test_case("\"chat\"@en", ""; "round for language string")]
#[test_case("042", "42"; "round for integer")]
#[test_case("3.14", "3.0"; "round for decimal")]
#[test_case("3.14e0", "3e0"; "round for double")]
#[test_case("\"1.5\"^^xsd:float", "\"2e0\"^^xsd:float"; "round for float")]
#[test_case("\"a\"^^xsd:integer", ""; "round for ill formed")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "round for triple")]
fn round(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::round(&arg2)), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::round(&arg1)), exp));
    Ok(())
}

#[test]
fn round_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    assert!(dbg!(super::round(&bnode)).is_none());
}

#[test_case("<tag:x>", ""; "concat for IRI")]
#[test_case("\"42\"", "\"42\""; "concat for string")]
#[test_case("\"chat\"@en", "\"chat\""; "concat for language string")]
#[test_case("042", ""; "concat for number")]
#[test_case("<< <tag:s> <tag:p> <tag:o> >>", ""; "concat for triple")]
fn concat(arg: &str, exp: &str) -> TestResult {
    let (arg1, arg2) = eval_expr(arg)?;
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?.0)
    };
    if let Some(arg2) = arg2 {
        assert!(eval_eq(dbg!(super::concat(&[arg2])), exp.clone()));
    }
    assert!(eval_eq(dbg!(super::concat(&[arg1])), exp));
    Ok(())
}

#[test]
fn concat_for_bnode() {
    let bnode = EvalResult::from(BnodeId::new_unchecked(Arc::<str>::from("b")));
    assert!(dbg!(super::concat(&[bnode])).is_none());
}

#[test_case(vec![], "")]
#[test_case(vec!["a"], "a")]
#[test_case(vec!["a", "b"], "ab")]
#[test_case(vec!["a", "b", "c"], "abc")]
#[test_case(vec!["a", "b", "c", "d"], "abcd")]
fn concat_var_args(args: Vec<&str>, exp: &str) {
    let args: Vec<_> = args
        .into_iter()
        .map(|txt| EvalResult::from(Arc::<str>::from(txt)))
        .collect();
    let exp = Some(EvalResult::from(Arc::<str>::from(exp)));
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
