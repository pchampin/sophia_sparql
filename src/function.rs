use std::sync::Arc;

use rand::random;
use sophia::{
    api::{
        ns::xsd,
        term::{BnodeId, IriRef, LanguageTag, Term},
    },
    term::{ArcTerm, GenericLiteral},
};
use spargebra::algebra::Function::{self, *};

use crate::{
    expression::EvalResult,
    ns::RDF_LANG_STRING,
    value::{SparqlNumber, SparqlValue},
    ResultTerm,
};

pub fn call_function(function: &Function, mut arguments: Vec<EvalResult>) -> Option<EvalResult> {
    match function {
        Str => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            if let Some(iri) = arg.as_iri() {
                str_iri(iri)
            } else if let Some(lit) = arg.as_literal() {
                str_literal(lit)
            } else {
                None
            }
        }
        Lang => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            lang(arg)
        }
        Datatype => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            datatype(arg)
        }
        Iri => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            iri(arg)
        }
        BNode => {
            let arg = arguments.pop();
            bnode(arg.as_ref())
        }
        Rand => rand(),
        Abs => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            abs(arg)
        }
        Ceil => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            ceil(arg)
        }
        Floor => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            floor(arg)
        }
        Round => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            round(arg)
        }
        Concat => concat(&arguments),
        LangMatches => {
            let [tag, range] = &arguments[..] else {
                unreachable!()
            };
            lang_matches(tag, range)
        }
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

pub fn str_iri(iri: &IriRef<Arc<str>>) -> Option<EvalResult> {
    Some(iri.clone().unwrap().into())
}

pub fn str_literal(lit: GenericLiteral<Arc<str>>) -> Option<EvalResult> {
    use GenericLiteral::*;
    match lit {
        Typed(lex, _) => Some(lex.into()),
        LanguageString(lex, _) => Some(lex.into()),
    }
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

pub fn lang(er: &EvalResult) -> Option<EvalResult> {
    use GenericLiteral::{LanguageString, Typed};
    match er {
        EvalResult::Term(rt) => match rt.inner() {
            ArcTerm::Literal(LanguageString(_, tag)) => Some(tag.clone().unwrap().into()),
            ArcTerm::Literal(Typed(..)) => Some(Arc::<str>::from("").into()),
            _ => None,
        },
        EvalResult::Value(SparqlValue::String(_, Some(tag))) => Some(tag.clone().unwrap().into()),
        EvalResult::Value(_) => Some(Arc::<str>::from("").into()),
    }
}

pub fn datatype(er: &EvalResult) -> Option<EvalResult> {
    use GenericLiteral::{LanguageString, Typed};
    match er {
        EvalResult::Term(rt) => match rt.inner() {
            ArcTerm::Literal(LanguageString(..)) => Some(RDF_LANG_STRING.clone().into()),
            ArcTerm::Literal(Typed(_, dt)) => Some(dt.clone().into()),
            _ => None,
        },
        EvalResult::Value(value) => Some(value.datatype().into()),
    }
}

pub fn iri(er: &EvalResult) -> Option<EvalResult> {
    use GenericLiteral::Typed;
    match er {
        EvalResult::Term(rt) => match rt.inner() {
            ArcTerm::Iri(_) => Some(er.clone()),
            ArcTerm::Literal(Typed(lex, dt)) if xsd::string == dt => {
                IriRef::new(lex.clone()).ok().map(|iri| iri.into())
            }
            _ => None,
        },
        EvalResult::Value(SparqlValue::String(lex, None)) => {
            IriRef::new(lex.clone()).ok().map(|iri| iri.into())
        }
        _ => None,
    }
}

pub fn bnode(opt: Option<&EvalResult>) -> Option<EvalResult> {
    if let Some(er) = opt {
        // mimic Jena for the moment: ignore the argument
        // because we don't know whether we are in the same result or not.
        // TODO improve compliance and generate same bnode for a given 'er' AND result number?
        er.as_simple().and_then(|_| bnode(None))
    } else {
        let bnid = uuid::Uuid::now_v7().to_string();
        let bnid = BnodeId::<Arc<str>>::new_unchecked(bnid.into());
        Some(bnid.into())
    }
}

pub fn rand() -> Option<EvalResult> {
    Some(SparqlNumber::from(random::<f64>()).into())
}

pub fn abs(er: &EvalResult) -> Option<EvalResult> {
    er.as_number().and_then(SparqlNumber::abs).map(Into::into)
}

pub fn ceil(er: &EvalResult) -> Option<EvalResult> {
    er.as_number().and_then(SparqlNumber::ceil).map(Into::into)
}

pub fn floor(er: &EvalResult) -> Option<EvalResult> {
    er.as_number().and_then(SparqlNumber::floor).map(Into::into)
}

pub fn round(er: &EvalResult) -> Option<EvalResult> {
    er.as_number().and_then(SparqlNumber::round).map(Into::into)
}

pub fn concat(ers: &[EvalResult]) -> Option<EvalResult> {
    ers.iter()
        .map(|er| er.as_string().map(|pair| pair.0.as_ref()))
        .collect::<Option<Vec<_>>>()
        .map(|args| EvalResult::from(Arc::<str>::from(args.join(""))))
}

pub fn lang_matches(tag: &EvalResult, range: &EvalResult) -> Option<EvalResult> {
    let tag = LanguageTag::new(tag.as_simple()?.clone()).ok()?;
    let range = range.as_simple()?;
    if range.as_ref() == "*" {
        return Some(true.into());
    }
    let range = LanguageTag::new(range.clone()).ok()?;
    return Some(
        (range.len() <= tag.len()
            && tag[..range.len()].eq_ignore_ascii_case(range.as_str())
            && (tag.len() == range.len() || tag[range.len()..].starts_with('-')))
        .into(),
    );
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

#[cfg(test)]
mod test;
