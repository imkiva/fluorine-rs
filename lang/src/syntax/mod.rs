use crate::syntax::{
    optimize::{OptimizeLevel, Optimizer},
    parse::{CompileError, FsParser},
    tree::Program,
};

pub mod optimize;
pub mod parse;
pub mod pe;
pub mod tree;

pub struct Compiler {}

impl Compiler {
    pub fn compile(level: OptimizeLevel, input: &str) -> Result<Program, CompileError> {
        FsParser::ast(input).map(|ast| Optimizer::run(ast, level))
    }
}
