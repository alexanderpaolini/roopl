mod lex;
mod token;

fn main() {
    let toks = lex::lex("aa \"str\" aa".to_string());

    for tok in toks {
        println!("{0}", tok);
    }
}
