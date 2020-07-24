use crate::syntax::tree::{Expr, Param, Program};

pub fn desugar_async_lambda(_params: Vec<Param>, _body: Vec<Expr>, _extra: &mut Program) -> Expr {
    todo!()

}
