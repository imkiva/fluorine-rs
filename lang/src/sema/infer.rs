use crate::{
    sema::{Context, Infer, Result, Type},
    syntax::tree::Expr,
};

impl Infer for Expr {
    fn infer_type(&self, _ctx: &Context) -> Result<Type> {
        Ok(Type::UnitType)
    }
}
