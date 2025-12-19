use clap::Parser;

mod ast;
mod lex;
mod parse;
mod resolver;
mod semantics;
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

    let entry = args.input;

    let resolver = match resolver::ModuleResolver::new(entry) {
        Ok(resolver) => resolver,
        Err(e) => {
            eprintln!("{:?}", e);
            return;
        }
    };

    let res = resolver.resolve();

    if let Err(errors) = res {
        for error in errors {
            eprintln!("{:?}", error);
        }
        return;
    }

    let program = res.unwrap();

    if args.ast {
        println!("{program}");
    }

    if let Err(errors) = semantics::check(program) {
        for error in errors {
            eprintln!("{:?}", error);
        }
    }
}
