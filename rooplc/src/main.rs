use clap::Parser;
use std::fs;

mod ast;
mod lex;
mod parse;
mod token;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The input file location. Generally Main.roopl.
    input: String,

    /// Print the AST
    #[arg(short, long)]
    ast: bool,

    /// Print the tokens list
    #[arg(short, long)]
    toks: bool,
}

fn main() {
    let args = Args::parse();

    let path = args.input;
    let contents = fs::read_to_string(&path).expect(&format!("error: could not read {}", path));

    let toks = lex::lex(contents);
    if args.toks {
        for tok in &toks {
            println!("{}", tok);
        }
    }

    let ast = parse::parse(toks);
    if args.ast {
        println!("{}", ast);
    }
}
