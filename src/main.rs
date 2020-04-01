use script::parser::FsParser;
use script::optimizer::Optimizer;

pub fn main() {
    let t = FsParser::ast(
        "let id = {a -> a}\n \
               let a = 1 > 20\n \
               let b = fuck_a && fuck_b\n \
               let c = (a) \n \
               let str = \"fuck huawei\"\n \
               let quick_lambda = _ + _ \n\
               let bool = true && false\n \
               let res = f(a, b, c, d)(e)"
    ).unwrap();

    println!("{:#?}", t);

    let o = Optimizer::run(t);
    println!();
    println!();
    println!("{:#?}", o);
}

