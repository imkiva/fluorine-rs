use crate::syntax::{
    desugar::Desugar,
    optimize::{OptimizeLevel, Optimizer},
    parse::{CompileError, FsParser},
    tree::Program,
};

extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lang_ffi_macro;

#[macro_use]
pub mod ffi;
pub mod codegen;
pub mod runtime;
pub mod sema;
pub mod syntax;

pub struct Compiler;

impl Compiler {
    pub fn compile(level: OptimizeLevel, input: &str) -> Result<Program, CompileError> {
        FsParser::ast(input)
            .map(|ast| Optimizer::run(ast, level))
            .map(|ast| Desugar::run(ast))
    }
}
