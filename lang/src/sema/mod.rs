pub use crate::runtime::Context;
pub use crate::runtime::Type;

pub enum SemaError {}

pub type Result<T> = std::result::Result<T, SemaError>;

pub trait Infer {
    fn infer_type(&self, ctx: &Context) -> Result<Type>;
}

mod infer;
