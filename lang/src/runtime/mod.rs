use crate::{
    codegen::{fs::FsCodeGenerator, PartialCodeGenerator},
    runtime::Value::{
        BoolValue, EnumCtor, EnumValue, LambdaValue, NumberValue, StringValue, UnitValue,
    },
    syntax::tree::{
        ApplyStartDBI, Argc, Atom::AtomLambda, EnumVariant, Expr, Expr::AtomExpr, Name,
    },
};
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::Formatter,
};

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
    EnumCtor(EnumVariant, ApplyStartDBI, Vec<Value>),
    EnumValue(EnumVariant, Vec<Value>),
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
            EnumCtor(variant, _, fields) | EnumValue(variant, fields) => {
                write!(f, "{}(", variant.name.as_str())?;
                let mut item = Vec::with_capacity(variant.fields as usize);
                for field in fields {
                    item.push(format!("{}", field));
                }
                for _ in item.len()..variant.fields as usize {
                    item.push("_".to_owned());
                }
                write!(f, "{})", item.join(","))
            }
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
