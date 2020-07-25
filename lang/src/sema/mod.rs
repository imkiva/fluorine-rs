pub use crate::runtime::{Context, Type};

pub enum SemaError {}

pub type Result<T> = std::result::Result<T, SemaError>;

pub trait Infer {
    fn infer_type(&self, ctx: &Context) -> Result<Type>;
}

mod infer;
