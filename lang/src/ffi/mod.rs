use crate::runtime::{FromValue, IntoValue, RuntimeError, Type, Value};
use std::{
    collections::VecDeque,
    fmt::{Debug, Formatter},
};

pub type FFIParam = VecDeque<Value>;
pub type FFIResult = Result<Value, FFIError>;
pub type FFIFn = fn(FFIParam) -> FFIResult;

pub struct FFIClosure {
    pub argc: usize,
    pub closure: FFIFn,
}

#[derive(Debug)]
pub enum FFIError {
    // arg-index, expected, got
    ArgTypeMismatch(usize, String, Type),
    CustomError(String),
    Panic(String),
}

pub trait FFIIntoValue {
    fn ffi_into_value(self) -> FFIResult;
}

pub trait FFIFromValue
where
    Self: Sized,
{
    fn ffi_from_value(
        value: Value,
        index: usize,
        expected: String,
        got: Type,
    ) -> Result<Self, FFIError>;
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

impl Into<RuntimeError> for FFIError {
    fn into(self) -> RuntimeError {
        RuntimeError::FFIError(self)
    }
}

impl std::fmt::Display for FFIError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FFIError::ArgTypeMismatch(i, expected, got) => write!(
                f,
                "TypeError: Argument {}: expected '{}', but got '{}'",
                i, expected, got
            ),
            FFIError::CustomError(reason) => write!(f, "RuntimeError: {}", reason),
            FFIError::Panic(reason) => write!(f, "panic: {}", reason),
        }
    }
}

impl FFIIntoValue for FFIError {
    fn ffi_into_value(self) -> FFIResult {
        Err(self)
    }
}

impl<T> FFIIntoValue for Result<T, FFIError>
where
    T: IntoValue,
{
    fn ffi_into_value(self) -> FFIResult {
        self.map(|t| t.into_value())
    }
}

impl<T> FFIIntoValue for T
where
    T: IntoValue,
{
    fn ffi_into_value(self) -> FFIResult {
        Ok(self.into_value())
    }
}

impl<T> FFIFromValue for T
where
    T: FromValue,
{
    fn ffi_from_value(
        value: Value,
        index: usize,
        expected: String,
        got: Type,
    ) -> Result<Self, FFIError> {
        Self::from_value(value).ok_or(FFIError::ArgTypeMismatch(index, expected, got))
    }
}
