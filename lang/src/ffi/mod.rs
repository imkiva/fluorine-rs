use crate::runtime::Value;
use std::fmt::{Debug, Formatter};

pub type FFIParam = Vec<Value>;
pub type FFIResult = Value;
pub type FFIFn = fn(FFIParam) -> FFIResult;

pub struct FFIType {
    pub closure: FFIFn,
}

impl Copy for FFIType {}

impl Debug for FFIType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FFIType")
    }
}

impl Clone for FFIType {
    fn clone(&self) -> Self {
        *self
    }
}

impl FFIType {
    pub fn boxed(closure: FFIFn) -> Box<Self> {
        Box::new(Self { closure })
    }
}
