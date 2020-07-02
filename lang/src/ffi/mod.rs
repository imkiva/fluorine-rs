use crate::runtime::Value;
use std::fmt::{Debug, Formatter};

pub type FFIParam = Vec<Value>;
pub type FFIResult = Value;
pub type FFIFn = fn(FFIParam) -> FFIResult;

pub struct FFIClosure {
    pub argc: usize,
    pub closure: FFIFn,
}

impl Copy for FFIClosure {}

impl Debug for FFIClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FFIType")
    }
}

impl Clone for FFIClosure {
    fn clone(&self) -> Self {
        *self
    }
}

impl FFIClosure {
    pub fn new(argc: usize, closure: FFIFn) -> Self {
        Self { argc, closure }
    }

    pub fn boxed(argc: usize, closure: FFIFn) -> Box<Self> {
        Box::new(Self::new(argc, closure))
    }
}
