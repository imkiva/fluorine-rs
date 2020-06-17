use crate::runtime::Value;

pub type FFIParam = Vec<Value>;
pub type FFIResult = Value;
pub type FFIType = dyn Fn(FFIParam) -> FFIResult;


