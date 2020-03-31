use script::parser::FsParser;

pub fn main() {
    let t = FsParser::ast("let id = {a -> a}").unwrap();
    println!("{:?}", t);
}

