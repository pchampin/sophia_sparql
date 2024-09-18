use crate::*;
use sophia::{
    api::{prelude::*, sparql::Query},
    inmem::dataset::LightDataset,
};
use test_case::test_case;

#[test_case(
    "SELECT ?x { ?s a ?x }",
    vec!["<http://schema.org/Event>", "<http://schema.org/Person>", ];
    "types"
)]
#[test_case(
    "SELECT ?x { [] ?x [] }",
    vec!["<http://schema.org/name>", "<http://schema.org/name>", "<http://schema.org/performerIn>", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", ];
    "predicates"
)]
fn test_select_1(query: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(query)?;
    let bindings = dataset.query(&query)?.into_bindings();
    assert_eq!(bindings.variables(), &["x"]);
    let mut got = bindings_to_vec(bindings);
    got.sort();
    assert_eq!(exp, got);
    Ok(())
}

#[test_case(1)]
#[test_case(2)]
#[test_case(3)]
#[test_case(4)]
#[test_case(5)]
#[test_case(6)]
fn test_limit_offset(limit: usize) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query0 = format!("SELECT ?p {{ [] ?p [] }} LIMIT {limit}");
    let got = bindings_to_vec(dataset.query(query0.as_str())?.into_bindings());
    assert_eq!(got.len(), limit.min(5));

    let mut offset = 0;
    let mut got = vec![];
    loop {
        let query = format!("SELECT ?p {{ [] ?p [] }} OFFSET {offset} LIMIT {limit}");
        let partial = bindings_to_vec(dataset.query(query.as_str())?.into_bindings());
        let exp_len = if offset >= 5 {
            0
        } else {
            limit.min(5 - offset)
        };
        assert_eq!(partial.len(), exp_len);
        got.extend_from_slice(&partial);
        if exp_len == 0 {
            break;
        } else {
            offset += limit;
        }
    }
    got.sort();
    let exp = vec![
        "<http://schema.org/name>",
        "<http://schema.org/name>",
        "<http://schema.org/performerIn>",
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
    ];
    assert_eq!(exp, got);
    Ok(())
}

#[test_case("", vec!["<https://example.org/test#a>", "_:b"]; "control")]
#[test_case("FILTER (true)", vec!["<https://example.org/test#a>", "_:b"]; "always true")]
#[test_case("FILTER (false)", vec![]; "always false")]
#[test_case("FILTER (42/0)", vec![]; "error")]
#[test_case("FILTER EXISTS { ?x s:name ?e }", vec!["<https://example.org/test#a>", "_:b"]; "exists redundant")]
#[test_case("FILTER EXISTS { ?x s:performerIn ?e }", vec!["<https://example.org/test#a>"]; "exists success")]
#[test_case("FILTER EXISTS { ?x s:knows ?e }", vec![]; "exists failure")]
#[test_case("FILTER EXISTS { BIND(42 as ?x) }", vec![]; "exists error")]
fn test_filter(filter: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX s: <http://schema.org/> SELECT ?x {{ ?x s:name ?n. {filter} }}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings);
    got.sort();
    assert_eq!(exp, got);
    Ok(())
}

#[test]
fn test_expr_iri() -> TestResult {
    assert_eq!(
        eval_expr("<http://schema.org/name>")?,
        "<http://schema.org/name>"
    );
    Ok(())
}

#[test]
fn test_expr_literal() -> TestResult {
    assert_eq!(
        eval_expr("42")?,
        "\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>"
    );
    Ok(())
}

#[test]
fn test_expr_variable() -> TestResult {
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse("SELECT (?y as ?x) { BIND(<http://schema.org/name> as ?y)}")?;
    let bindings = dataset.query(&query)?.into_bindings();
    let got = bindings_to_vec(bindings);
    assert_eq!(&["<http://schema.org/name>"], &got[..]);
    Ok(())
}

// test ||
#[test_case("true    || true   ", "true "; "T or T")]
#[test_case("true    || false  ", "true "; "T or F")]
#[test_case("false   || true   ", "true "; "F or T")]
#[test_case("false   || false  ", "false"; "F or F")]
#[test_case("true    || <tag:x>", "true "; "T or E")]
#[test_case("<tag:x> || true   ", "true "; "E or T")]
#[test_case("false   || <tag:x>", ""; "F or E")]
#[test_case("<tag:x> || false  ", ""; "E or F")]
#[test_case("<tag:x> || <tag:x>", ""; "E or E")]
// test &&
#[test_case("true    && true   ", "true "; "T and T")]
#[test_case("true    && false  ", "false"; "T and F")]
#[test_case("false   && true   ", "false"; "F and T")]
#[test_case("false   && false  ", "false"; "F and F")]
#[test_case("false   && <tag:x>", "false"; "F and E")]
#[test_case("<tag:x> && false  ", "false"; "E and F")]
#[test_case("true    && <tag:x>", ""; "T and E")]
#[test_case("<tag:x> && true   ", ""; "E and T")]
#[test_case("<tag:x> && <tag:x>", ""; "E and E")]
// test !
#[test_case("!true   ", "false"; "not T")]
#[test_case("!false  ", "true"; "not F")]
#[test_case("!<tag:x>", ""; "not E")]
// test Effective Boolean Value
#[test_case("!(!\"foo\")           ", "true "; "bool string non-empty")]
#[test_case("!(!\"\")              ", "false"; "bool string empty")]
#[test_case("!(!42)                ", "true "; "bool number non-zero")]
#[test_case("!(!0)                 ", "false"; "bool number zero")]
#[test_case("!(!\"foo\"@en)        ", "true "; "bool lang-string non-empty")]
#[test_case("!(!\"\"@en)           ", "false"; "bool lang-string empty")]
#[test_case("!(!\"x\"^^xsd:boolean)", "false"; "bool ill-formed")]
#[test_case("!(!<tag:x>)           ", ""; "bool iri")]
// test add
#[test_case("40+2", "42"; "add int")]
#[test_case("40+2.0", "42.0"; "add dec")]
#[test_case("40+\"2\"^^xsd:float", "\"4.2e1\"^^xsd:float"; "add flt")]
#[test_case("40+2e0", "4.2e1"; "add dbl")]
#[test_case("100000000000000000000+2", "100000000000000000002"; "add bigint")]
#[test_case("40+\"2\"", ""; "add err")]
// test sub
#[test_case("40-2", "38"; "sub int")]
#[test_case("40-2.0", "38.0"; "sub dec")]
#[test_case("40-\"2\"^^xsd:float", "\"3.8e1\"^^xsd:float"; "sub flt")]
#[test_case("40-2e0", "3.8e1"; "sub dbl")]
#[test_case("100000000000000000000-2", "99999999999999999998"; "sub bigint")]
#[test_case("40-\"2\"", ""; "sub err")]
// test mul
#[test_case("40*2", "80"; "mul int")]
#[test_case("40*2.0", "80.0"; "mul dec")]
#[test_case("40*\"2\"^^xsd:float", "\"8e1\"^^xsd:float"; "mul flt")]
#[test_case("40*2e0", "8e1"; "mul dbl")]
#[test_case("100000000000000000000*2", "200000000000000000000"; "mul bigint")]
#[test_case("40*\"2\"", ""; "mul err")]
// test div
#[test_case("40/2", "20.0"; "div int")]
#[test_case("40/2.0", "20.0"; "div dec")]
#[test_case("40/\"2\"^^xsd:float", "\"2e1\"^^xsd:float"; "div flt")]
#[test_case("40/2e0", "2e1"; "div dbl")]
#[test_case("100000000000000000000/2", "50000000000000000000.0"; "div bigint")]
#[test_case("40/\"2\"", ""; "div err")]
#[test_case("40/0", ""; "div by zero")]
// test unary-plus
#[test_case("+(42)", "42"; "plus int")]
#[test_case("+(42.0)", "42.0"; "plus dec")]
#[test_case("+(\"42\"^^xsd:float)", "\"4.2e1\"^^xsd:float"; "plus flt")]
#[test_case("+(42e0)", "4.2e1"; "plus dbl")]
#[test_case("+(100000000000000000000)", "100000000000000000000"; "plus bigint")]
#[test_case("+(\"42\")", ""; "plus str")]
#[test_case("+(42/0)", ""; "plus error")]
// test unary-minus
#[test_case("-(42)", "-42"; "minus int")]
#[test_case("-(42.0)", "-42.0"; "minus dec")]
#[test_case("-(\"42\"^^xsd:float)", "-\"4.2e1\"^^xsd:float"; "minus flt")]
#[test_case("-(42e0)", "-4.2e1"; "minus dbl")]
#[test_case("-(100000000000000000000)", "-100000000000000000000"; "minus bigint")]
#[test_case("-(\"42\")", ""; "minus str")]
#[test_case("-(42/0)", ""; "minus error")]
// test if-then-else
#[test_case("if(true, \"foo\", \"bar\")", "\"foo\""; "if-then-else true")]
#[test_case("if(false, \"foo\", \"bar\")", "\"bar\""; "if-then-else false")]
#[test_case("if(\"baz\", \"foo\", \"bar\")", "\"foo\""; "if-then-else truthy string")]
#[test_case("if(\"\", \"foo\", \"bar\")", "\"bar\""; "if-then-else empty string")]
// test in
#[test_case("42 in (12, 22, 32, 42, 52)", "true"; "in success")]
#[test_case("42 in (62, 72, 82, 92, 12)", "false"; "in failure")]
#[test_case("42 in ()", "false"; "in empty list")]
#[test_case("42 in (42.0)", "true"; "in with numeric coercion")]
#[test_case("42 in (1/0, 42)", ""; "in with error")]
#[test_case("<tag:3> in (<tag:1>, <tag:2>, <tag:3>, <tag:4>)", "true"; "in success with IRIs")]
#[test_case("<tag:3> in (<tag:5>, <tag:6>, <tag:7>, <tag:8>)", "false"; "in failure with IRIs")]
#[test_case("<tag:3> in (\"tag:3\")", "false"; "in does not mix IRIs and strings")]
// test coalesce
#[test_case("coalesce(1, 2, 3)", "1"; "coalesce first")]
#[test_case("coalesce(1/0, 2, -\"3\")", "2"; "coalesce middle")]
#[test_case("coalesce(1/0, -\"2\", 3)", "3"; "coalesce last")]
#[test_case("coalesce(1/0, -\"2\", !(<tag:3>))", ""; "coalesce none")]
// test is_iri
#[test_case("isIri(<tag:x>)", "true"; "isIri true")]
#[test_case("isIri(42)", "false"; "isIri false")]
#[test_case("isIri(42/0)", ""; "isIri error")]
// test is_literal
#[test_case("isLiteral(<tag:x>)", "false"; "isLiteral false")]
#[test_case("isLiteral(42)", "true"; "isLiteral true")]
#[test_case("isLiteral(42/0)", ""; "isLiteral error")]
// test isNumeric
#[test_case("isNumeric(<tag:x>)", "false"; "isNumeric false for IRI")]
#[test_case("isNumeric(\"42\")", "false"; "isNumeric false for string")]
#[test_case("isNumeric(42)", "true"; "isNumeric true")]
#[test_case("isNumeric(42/0)", ""; "isNumeric error")]
// test str
#[test_case("str(<tag:x>)", "\"tag:x\""; "str for IRI")]
#[test_case("str(\"42\")", "\"42\""; "str for string")]
#[test_case("str(\"chat\"@en)", "\"chat\""; "str for language string")]
#[test_case("str(042)", "\"042\""; "str for number")]
#[test_case("str(<< <tag:s> <tag:p> <tag:o> >>)", "\"<< <tag:s> <tag:p> <tag:o> >>\""; "str for triple")]
#[test_case("str(42/0)", ""; "str error")]
// TODO test other function calls
fn test_expr(expr: &str, result: &str) -> TestResult {
    let exp = if result.is_empty() {
        "".into()
    } else {
        eval_expr(result)?
    };
    assert_eq!(eval_expr(dbg!(expr))?, exp);
    Ok(())
}

#[test_case("42", "042", Some(true))]
#[test_case("42", "42.0", Some(true))]
#[test_case("42", "42e0", Some(true))]
#[test_case("42.0", "42e0", Some(true))]
#[test_case("42", "43", Some(false))]
#[test_case("\"a\"", "\"\"", Some(false))]
#[test_case("\"a\"@en", "\"\"@en", Some(false))]
#[test_case("\"a\"@en", "\"a\"@fr", Some(false))]
#[test_case("true", "false", Some(false))]
#[test_case(
    "\"2024-03-25T00:00:00\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+00:00\"^^xsd:dateTime",
    None
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+00:00\"^^xsd:dateTime",
    Some(true)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T01:00:00+01:00\"^^xsd:dateTime",
    Some(true)
)]
#[test_case(
    "\"2024-03-25T00:00:00\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01\"^^xsd:dateTime",
    Some(false)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01Z\"^^xsd:dateTime",
    Some(false)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+01:00\"^^xsd:dateTime",
    Some(false)
)]
#[test_case("<tag:x>", "<tag:y>", Some(false))]
#[test_case("\"a\"^^<tag:x>", "\"a\"^^<tag:y>", None)]
#[test_case("\"a\"^^<tag:x>", "\"b\"^^<tag:x>", None)]
fn test_expr_eq(expr1: &str, expr2: &str, exp: Option<bool>) -> TestResult {
    dbg!(expr1, expr2);
    // control: every term is equal to itself
    assert_eq!(eval_expr(&format!("{expr1} = {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} != {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} = {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} != {expr2}"))?, FALSE);
    // control: every recognized value is equal to itself via comparison operators
    if !expr1.contains("<tag:") {
        assert_eq!(eval_expr(&format!("{expr1} <= {expr1}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} >= {expr1}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} < {expr1}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr1} > {expr1}"))?, FALSE);

        assert_eq!(eval_expr(&format!("{expr2} <= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr2} >= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr2} < {expr2}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr2} > {expr2}"))?, FALSE);
    }

    let (exp_eq, exp_neq) = match exp {
        Some(true) => (TRUE, FALSE),
        Some(fale) => (FALSE, TRUE),
        None => ("", ""),
    };
    assert_eq!(eval_expr(&format!("{expr1} = {expr2}"))?, exp_eq);
    assert_eq!(eval_expr(&format!("{expr1} != {expr2}"))?, exp_neq);
    assert_eq!(eval_expr(&format!("sameTerm({expr1}, {expr2})"))?, FALSE);
    assert_eq!(eval_expr(&format!("sameTerm({expr1}, {expr1})"))?, TRUE);
    assert_eq!(eval_expr(&format!("sameTerm({expr2}, {expr2})"))?, TRUE);
    if exp == Some(true) {
        assert_eq!(eval_expr(&format!("{expr1} <= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} >= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} < {expr2}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr1} > {expr2}"))?, FALSE);
    }
    Ok(())
}

#[test_case("42", "43")]
#[test_case("42", "43.0")]
#[test_case("42", "43e0")]
#[test_case("42.0", "43e0")]
#[test_case("\"\"", "\"a\"")]
#[test_case("\"a\"", "\"ab\"")]
#[test_case("\"a\"", "\"b\"")]
#[test_case("\"10\"", "\"2\"")]
#[test_case("\"\"@en", "\"a\"@en")]
#[test_case("\"a\"@en", "\"ab\"@en")]
#[test_case("\"a\"@en", "\"b\"@en")]
#[test_case("\"10\"@en", "\"b\"@en")]
#[test_case("false", "true")]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01Z\"^^xsd:dateTime"
)]
fn test_expr_lt(expr1: &str, expr2: &str) -> TestResult {
    assert_eq!(eval_expr(&format!("{expr1} < {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} <= {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} != {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} > {expr2}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr1} >= {expr2}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr1} = {expr2}"))?, FALSE);
    //
    assert_eq!(eval_expr(&format!("{expr2} < {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} <= {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} != {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} > {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} >= {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} = {expr1}"))?, FALSE);
    Ok(())
}

#[test]
fn test_is_blank() -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(
        "PREFIX s: <http://schema.org/> SELECT ?x {{ ?x s:name ?n. FILTER (isBlank(?x)) }}",
    )?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings);
    got.sort();
    assert_eq!(vec!["_:b"], got);
    Ok(())
}

fn eval_expr(expr: &str) -> TestResult<String> {
    eprintln!("eval_expr: {expr}");
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings);
    assert_eq!(got.len(), 1);
    Ok(got.pop().unwrap())
}

#[test_case("BIND(42 as ?x)", TRUE; "x is bound")]
#[test_case("", FALSE; "nothing bound")]
#[test_case("BIND(42 as ?x2)", FALSE; "x is not bound")]
#[test_case("BIND(42/0 as ?x)", FALSE; "x gets an error")]
fn test_bound(body: &str, exp: &str) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let got = bindings_to_vec(
        dataset
            .query(format!("SELECT (BOUND(?x) as ?b) {{ {body} }}").as_str())?
            .into_bindings(),
    );
    assert_eq!(got.len(), 1);
    assert_eq!(&got[0], exp);
    Ok(())
}

const TRUE: &str = "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>";
const FALSE: &str = "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>";

fn dataset_101() -> TestResult<LightDataset> {
    let dataset: LightDataset = sophia::turtle::parser::trig::parse_str(
        r#"
                BASE <https://example.org/test>
                PREFIX s: <http://schema.org/>

                <#a> a s:Person ;
                  s:name "Alice" ;
                  s:performerIn [
                    a s:Event ;
                    s:name "Bob's birthday party" ;
                  ].

            "#,
    )
    .collect_quads()?;
    Ok(dataset)
}

fn bindings_to_vec(bindings: Bindings<LightDataset>) -> Vec<String> {
    assert_eq!(bindings.variables().len(), 1);
    bindings
        .into_iter()
        .map(|b| {
            b.unwrap()[0]
                .as_ref()
                .map(|t| {
                    if t.is_blank_node() {
                        "_:b".to_string()
                    } else {
                        t.to_string()
                    }
                })
                .unwrap_or("".into())
        })
        .collect()
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;
