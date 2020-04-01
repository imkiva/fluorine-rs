use script::parser::FsParser;
use script::optimizer::Optimizer;

pub fn main() {
    let t = FsParser::ast(
        "//let id = {a -> a}\n \
               //let a = 1 > 20\n \
               //let b = fuck_a && fuck_b\n \
               //let c = (a) \n \
               //let str = \"fuck huawei\"\n \
               //let quick_lambda = _ + _ \n\
               //let bool = true && false\n \
               //let res = f(a, b, c, d)(e)\n \
               //let add = {a, b -> a + b}\n \
               //let lll = { n -> { a -> n + a } }\n \
               let constant = {10086}\n \
               let ide = { a -> a }(10)\n \
               let fold2 = { n -> n + 2 }(1)\n \
               let fold3 = { n -> { a -> n + a } }(5)(10)\n\
               let fold4 = {a, b -> a + b}(100, 200)\n\
               let fold5 = {a, b -> a + b}(100)"
    ).unwrap();

    println!("{:#?}", t);

    let o = Optimizer::run(t);
    println!();
    println!();
    println!("{:#?}", o);
}

