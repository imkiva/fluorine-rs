use crate::sema::Context;
use crate::sema::Infer;
use crate::sema::Result;
use crate::sema::Type;
use crate::syntax::tree::Expr;

impl Infer for Expr {
    fn infer_type(&self, _ctx: &Context) -> Result<Type> {
        Ok(Type::UnitType)
    }
}
