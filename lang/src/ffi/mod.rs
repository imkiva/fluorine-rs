use crate::runtime::Value;
use std::fmt::{Debug, Formatter};

pub type FFIParam = Vec<Value>;
pub type FFIResult = Value;
pub type FFIFn = fn(FFIParam) -> FFIResult;

pub struct FFIType {
    pub argc: usize,
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
    pub fn new(argc: usize, closure: FFIFn) -> Self {
        Self { argc, closure }
    }

    pub fn boxed(argc: usize, closure: FFIFn) -> Box<Self> {
        Box::new(Self::new(argc, closure))
    }
}
