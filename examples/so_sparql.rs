use std::{env::args, sync::Arc};

use sophia::{
    api::{
        prelude::MutableDataset,
        sparql::{SparqlDataset, SparqlResult},
    },
    inmem::dataset::FastDataset,
};
use sophia_sparql::*;

fn main() {
    let args: Vec<_> = args().collect();
    if args.len() < 2 || args.len() > 3 {
        eprintln!("usage: {} <SPARQL-QUERY> [N-QUAD-FILE]", args[0]);
        std::process::exit(1);
    }
    let query = &args[1];
    let mut dataset = FastDataset::new();
    if let Some(filename) = args.get(2) {
        let read = std::fs::File::open(filename).unwrap();
        let bufread = std::io::BufReader::new(read);
        let quads = sophia::turtle::parser::nq::parse_bufread(bufread);
        dataset.insert_all(quads).unwrap();
    }
    eprintln!(
        "AST: {:#?}",
        SparqlWrapper(&dataset)
            .prepare_query(query.as_str())
            .unwrap()
    );
    let res = SparqlWrapper(&dataset).query(query.as_str()).unwrap();
    match res {
        SparqlResult::Bindings(bs) => {
            let variables: Vec<Arc<str>> = bs.variables().into_iter().map(Into::into).collect();
            for b in bs {
                let b = b.unwrap();
                for (key, val) in variables.iter().zip(b.into_iter()) {
                    if let Some(val) = val {
                        println!("{key}= {val}");
                    } else {
                        println!("{key}=");
                    }
                }
                println!();
            }
        }
        SparqlResult::Boolean(true) => {
            println!("yes");
        }
        SparqlResult::Boolean(false) => {
            println!("no");
            std::process::exit(-1);
        }
        SparqlResult::Triples(_) => todo!("GRAPH result"),
    }
}
