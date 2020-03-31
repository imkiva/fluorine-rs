use script::parser::FsParser;

pub fn main() {
    let t = FsParser::to_tree("let id = {a -> a}").unwrap();
    println!("{:?}", t);
}

