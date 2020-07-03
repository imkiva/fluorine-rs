use crate::{
    codegen::{fs::FsCodeGenerator, PartialCodeGenerator},
    ffi::{FFIClosure, FFIError},
    runtime::Value::{
        BoolValue, EnumCtor, EnumValue, ForeignLambda, LambdaValue, NumberValue, StringValue,
        UnitValue,
    },
    syntax::tree::{
        ApplyStartDBI, Argc, Atom::AtomLambda, EnumVariant, Expr, Expr::AtomExpr, Name,
        PatEnumVariant,
    },
};
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Formatter,
};

mod builtins;
pub mod eval;
pub mod pattern;
pub mod subst;

#[derive(Debug)]
pub enum RuntimeError {
    StackUnderflow,
    VariableNotFound(String),
    NotApplicable,
    NonExhaustive,
    TypeMismatch,
    FFIError(FFIError),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::StackUnderflow => write!(f, "RuntimeError: stack underflow"),
            RuntimeError::VariableNotFound(id) => {
                write!(f, "NameError: variable '{}' not found", id)
            }
            RuntimeError::NotApplicable => write!(f, "TypeError: not a lambda"),
            RuntimeError::NonExhaustive => write!(f, "RuntimeError: non-exhaustive match rule"),
            RuntimeError::TypeMismatch => {
                write!(f, "TypeError: operators can only apply to same types")
            }
            RuntimeError::FFIError(err) => write!(f, "FFIError: {}", err),
        }
    }
}

#[derive(Debug)]
pub struct Context {
    pub stack: VecDeque<Scope>,
}

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<Name, Value>,
    pub enums: HashMap<Name, Vec<EnumVariant>>,
}

#[derive(Clone, Debug)]
pub enum Value {
    UnitValue,
    NumberValue(f64),
    BoolValue(bool),
    StringValue(String),
    LambdaValue(Argc, ApplyStartDBI, Vec<Expr>),
    EnumCtor(EnumType, EnumVariant, Vec<Value>),
    EnumValue(EnumType, EnumVariant, Vec<Value>),
    ForeignLambda(Argc, VecDeque<Value>, Box<FFIClosure>),
}

#[derive(Clone, Debug)]
pub enum Type {
    UnitType,
    NumberType,
    BoolType,
    StringType,
    LambdaType(Argc),
    EnumType(EnumType),
}

#[derive(Clone, Debug)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

pub trait IntoValue {
    fn into_value(self) -> Value;
}

pub trait FromValue
where
    Self: Sized,
{
    fn from_value(value: Value) -> Option<Self>;
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnitValue => write!(f, "() :: Unit"),
            NumberValue(v) => write!(f, "{} :: Number", v),
            BoolValue(v) => write!(f, "{} :: Bool", v),
            StringValue(v) => write!(f, "{} :: String", v),
            LambdaValue(argc, dbi, body) => {
                let gen = FsCodeGenerator::new();
                let code =
                    gen.partial_codegen_expr(AtomExpr(AtomLambda(*argc, *dbi, body.clone())));
                write!(f, "{}", code)
            }
            EnumCtor(ty, variant, fields) | EnumValue(ty, variant, fields) => {
                write!(f, "{}(", variant.name.as_str())?;
                let mut item = Vec::with_capacity(variant.fields as usize);
                for field in fields {
                    item.push(format!("{}", field));
                }
                for _ in item.len()..variant.fields as usize {
                    item.push("_".to_owned());
                }
                write!(f, "{}) :: {}", item.join(","), ty.name)
            }
            ForeignLambda(_, _, _) => write!(f, "<foreign-lambda>"),
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (NumberValue(lhs), NumberValue(rhs)) => lhs.partial_cmp(rhs),
            (StringValue(lhs), StringValue(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NumberValue(lhs), NumberValue(rhs)) => lhs == rhs,
            (BoolValue(lhs), BoolValue(rhs)) => lhs == rhs,
            (StringValue(lhs), StringValue(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl EnumType {
    pub fn has_variant(&self, variant: &EnumVariant) -> bool {
        self.variants.iter().position(|v| v == variant).is_some()
    }

    pub fn has_pat_variant(&self, variant: &PatEnumVariant) -> bool {
        self.variants
            .iter()
            .position(|v| v.name == variant.name && v.fields == variant.fields.len())
            .is_some()
    }
}

impl IntoValue for Value {
    fn into_value(self) -> Value {
        self
    }
}

impl FromValue for Value {
    fn from_value(value: Value) -> Option<Self> {
        Some(value)
    }
}

impl IntoValue for String {
    fn into_value(self) -> Value {
        StringValue(self)
    }
}

impl IntoValue for &str {
    fn into_value(self) -> Value {
        StringValue(self.to_owned())
    }
}

impl FromValue for String {
    fn from_value(value: Value) -> Option<Self> {
        match value {
            StringValue(str) => Some(str),
            _ => None,
        }
    }
}

impl FromValue for () {
    fn from_value(value: Value) -> Option<Self> {
        match value {
            UnitValue => Some(()),
            _ => None,
        }
    }
}

impl IntoValue for () {
    fn into_value(self) -> Value {
        UnitValue
    }
}

impl FromValue for bool {
    fn from_value(value: Value) -> Option<Self> {
        match value {
            BoolValue(b) => Some(b),
            _ => None,
        }
    }
}

impl IntoValue for bool {
    fn into_value(self) -> Value {
        BoolValue(self)
    }
}

macro_rules! to_value {
    ($t:ty) => {
        impl IntoValue for $t {
            fn into_value(self) -> Value {
                NumberValue(self as f64)
            }
        }
    };
}

macro_rules! from_value {
    ($t:ty) => {
        impl FromValue for $t {
            fn from_value(value: Value) -> Option<Self> {
                match value {
                    NumberValue(n) => Some(n as $t),
                    _ => None,
                }
            }
        }
    };
}

to_value!(i8);
to_value!(i16);
to_value!(i32);
to_value!(i64);
to_value!(u8);
to_value!(u16);
to_value!(u32);
to_value!(u64);
to_value!(f32);
to_value!(f64);

from_value!(i8);
from_value!(i16);
from_value!(i32);
from_value!(i64);
from_value!(u8);
from_value!(u16);
from_value!(u32);
from_value!(u64);
from_value!(f32);
from_value!(f64);
