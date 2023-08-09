//! Example of use:
//! ```
//! # use sophia::api::{prelude::*, sparql::*};
//! # use sophia_sparql::*;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let dataset: Vec<[i32; 4]> = vec![]; // dummy dataset
//! #
//! let dataset = SparqlWrapper(&dataset);
//! let query = SparqlQuery::parse("SELECT ?o { ?s a ?o }")?;
//! let bindings = dataset.query(&query)?.into_bindings();
//! for b in bindings {
//!     let b = b?;
//!     if let Some(o) = &b[0] {
//!         println!("found {o}");
//!     }
//! }
//! #
//! # Ok(()) }
//! ```

#![allow(dead_code, unused_variables)]
mod bgp;
mod binding;
mod exec;
mod matcher;
mod number;
mod term;
mod wrapper;

pub use binding::{BindingMap, Bindings};
pub use term::*;
pub use wrapper::*;

#[cfg(test)]
mod test {
    use crate::*;
    use sophia::api::{prelude::*, sparql::Query};

    #[test]
    fn test_101() -> Result<(), Box<dyn std::error::Error>> {
        let dataset: sophia::inmem::dataset::LightDataset =
            sophia::turtle::parser::trig::parse_str(
                r#"
                BASE <https://example.org/test>
                PREFIX s: <http://schema.org/>

                <#a> a s:Person ;
                  s:name "Alice" ;
                  s:attendee [
                    a s:Event ;
                    s:name "Bob's birthday party" ;
                  ].

            "#,
            )
            .collect_quads()?;
        let dataset = SparqlWrapper(&dataset);
        let query = SparqlQuery::parse("SELECT ?o { ?s a ?o }")?;
        let bindings = dataset.query(&query)?.into_bindings();
        assert_eq!(bindings.variables(), &["o"]);
        let mut got: Vec<_> = bindings
            .into_iter()
            .map(|b| b.unwrap()[0].as_ref().unwrap().to_string())
            .collect();
        got.sort();
        let exp = vec!["<http://schema.org/Event>", "<http://schema.org/Person>"];
        assert_eq!(exp, got);
        Ok(())
    }
}
