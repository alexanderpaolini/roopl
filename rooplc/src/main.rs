mod ast;
mod lex;
mod parse;
mod token;

fn main() {
    let toks = lex::lex("aa \"str\" aa".to_string());

    let _parsed = parse::parse(toks);
}
