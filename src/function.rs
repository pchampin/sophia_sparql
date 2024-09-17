use std::sync::Arc;

use sophia::{
    api::term::Term,
    term::{ArcTerm, GenericLiteral},
};
use spargebra::algebra::Function::{self, *};

use crate::{expression::EvalResult, value::SparqlValue, ResultTerm};

pub fn call_function(function: &Function, arguments: Vec<EvalResult>) -> Option<EvalResult> {
    match function {
        Str => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            str(arg)
        }
        Lang => todo("Lang"),
        LangMatches => todo("LangMatches"),
        Datatype => todo("Datatype"),
        Iri => todo("Iri"),
        BNode => todo("BNode"),
        Rand => todo("Rand"),
        Abs => todo("Abs"),
        Ceil => todo("Ceil"),
        Floor => todo("Floor"),
        Round => todo("Round"),
        Concat => todo("Concat"),
        SubStr => todo("SubStr"),
        StrLen => todo("StrLen"),
        Replace => todo("Replace"),
        UCase => todo("UCase"),
        LCase => todo("LCase"),
        EncodeForUri => todo("EncodeForUri"),
        Contains => todo("Contains"),
        StrStarts => todo("StrStarts"),
        StrEnds => todo("StrEnds"),
        StrBefore => todo("StrBefore"),
        StrAfter => todo("StrAfter"),
        Year => todo("Year"),
        Month => todo("Month"),
        Day => todo("Day"),
        Hours => todo("Hours"),
        Minutes => todo("Minutes"),
        Seconds => todo("Seconds"),
        Timezone => todo("Timezone"),
        Tz => todo("Tz"),
        Now => todo("Now"),
        Uuid => todo("Uuid"),
        StrUuid => todo("StrUuid"),
        Md5 => todo("Md5"),
        Sha1 => todo("Sha1"),
        Sha256 => todo("Sha256"),
        Sha384 => todo("Sha384"),
        Sha512 => todo("Sha512"),
        StrLang => todo("StrLang"),
        StrDt => todo("StrDt"),
        IsIri => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            is_iri(arg)
        }
        IsBlank => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            is_blank(arg)
        }
        IsLiteral => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            is_literal(arg)
        }
        IsNumeric => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            is_numeric(arg)
        }
        Regex => todo("Regex"),
        Triple => {
            let [s, p, o] = &arguments[..] else {
                unreachable!()
            };
            triple(s, p, o)
        }
        Subject => todo("Subject"),
        Predicate => todo("Predicate"),
        Object => todo("Object"),
        IsTriple => todo("IsTriple"),
        Custom(iri) => todo(iri.to_string()),
    }
}

pub fn str(er: &EvalResult) -> Option<EvalResult> {
    Some(
        match er.as_term() {
            ArcTerm::Iri(iri) => iri.unwrap(),
            ArcTerm::BlankNode(bnid) => format!("_:{}", bnid.as_str()).into(),
            ArcTerm::Literal(GenericLiteral::Typed(lex, ..)) => lex,
            ArcTerm::Literal(GenericLiteral::LanguageString(lex, ..)) => lex,
            ArcTerm::Variable(varname) => format!("?{}", varname.as_str()).into(), // should never happen in standard SPARQL
            ArcTerm::Triple(t) => {
                let mut buf: Vec<u8> = b"<< ".into();
                for term in t.iter() {
                    sophia::turtle::serializer::nt::write_term(&mut buf, term.borrow_term())
                        .ok()?;
                    buf.push(b' ');
                }
                buf.extend(b">>");
                Arc::from(String::from_utf8(buf).ok()?)
            }
        }
        .into(),
    )
}

pub fn is_iri(er: &EvalResult) -> Option<EvalResult> {
    Some(
        match er {
            EvalResult::Term(t) => t.is_iri(),
            EvalResult::Value(_) => false,
        }
        .into(),
    )
}

pub fn is_blank(er: &EvalResult) -> Option<EvalResult> {
    Some(
        match er {
            EvalResult::Term(t) => t.is_blank_node(),
            EvalResult::Value(_) => false,
        }
        .into(),
    )
}

pub fn is_literal(er: &EvalResult) -> Option<EvalResult> {
    Some(
        match er {
            EvalResult::Term(t) => t.is_literal(),
            EvalResult::Value(_) => true,
        }
        .into(),
    )
}

pub fn is_numeric(er: &EvalResult) -> Option<EvalResult> {
    Some(matches!(er.as_value(), Some(SparqlValue::Number(_))).into())
}

pub fn triple(s: &EvalResult, p: &EvalResult, o: &EvalResult) -> Option<EvalResult> {
    let EvalResult::Term(s) = s else { return None };
    let EvalResult::Term(p) = p else { return None };
    if !p.is_iri() {
        return None;
    };
    let o = ResultTerm::from(o.as_term());
    Some(ResultTerm::from([s.clone(), p.clone(), o.clone()]).into())
}

fn todo<T: std::fmt::Display>(function_name: T) -> Option<EvalResult> {
    eprintln!("Function not implemented: {function_name}");
    None
}
