extern crate pest;
#[macro_use]
extern crate pest_derive;

pub(crate) mod functor;
pub(crate) mod pattern;
pub(crate) mod pe;
pub(crate) mod subst;

pub mod codegen;
pub mod eval;
pub mod optimize;
pub mod parse;
pub mod tree;

mod tests;
