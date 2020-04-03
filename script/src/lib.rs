extern crate pest;
#[macro_use]
extern crate pest_derive;

pub(crate) mod functor;
pub(crate) mod subst;

pub mod parser;
pub mod optimizer;
pub mod tree;
pub mod eval;
pub mod codegen;
