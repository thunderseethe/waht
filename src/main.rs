mod ast;
mod lexer;
mod parser;

fn main() -> anyhow::Result<()> {
    let filename = env::args().nth(1).unwrap();

    let contents = std::fs::read_to_string(filename)?;

}
