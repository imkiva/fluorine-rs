use script::parser::FsParser;

pub fn main() {
    let t = FsParser::ast(
        "let id = {a -> a}\n \
               let a = 1 > 20\n \
               let b = fuck_a && fuck_b"
    ).unwrap();
    println!("{:?}", t);
}

