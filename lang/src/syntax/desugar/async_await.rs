use crate::syntax::tree::{Expr, Param, Program};

pub fn desugar_async_lambda(params: Vec<Param>, body: Vec<Expr>, extra: &mut Program) -> Expr {
    todo!()
}
