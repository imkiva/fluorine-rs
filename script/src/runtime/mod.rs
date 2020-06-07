use crate::{
    codegen::{fs::FsCodeGenerator, PartialCodeGenerator},
    runtime::Value::{BoolValue, LambdaValue, NumberValue, StringValue},
    syntax::tree::{ApplyStartDBI, Argc, Atom::AtomLambda, Expr, Expr::AtomExpr, Name},
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
    DanglingDBI,
    DanglingRawLambda,
    StackUnderflow,
    VariableNotFound(String),
    NotApplicable,
    NonExhaustive,
    BottomTypedValue,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::DanglingDBI => {
                write!(f, "InternalError: detected dangling DBI outside lambda")
            }
            RuntimeError::DanglingRawLambda => {
                write!(f, "InternalError: detected unresolved lambda")
            }
            RuntimeError::StackUnderflow => write!(f, "RuntimeError: stack underflow"),
            RuntimeError::VariableNotFound(id) => {
                write!(f, "NameError: variable '{}' not found", id)
            }
            RuntimeError::BottomTypedValue => {
                write!(f, "TypeError: try to produce bottom typed value")
            }
            RuntimeError::NotApplicable => write!(f, "TypeError: not a lambda"),
            RuntimeError::NonExhaustive => write!(f, "RuntimeError: non-exhaustive match rule"),
        }
    }
}

pub struct Context {
    stack: VecDeque<Scope>,
}

pub struct Scope {
    vars: HashMap<Name, Value>,
}

#[derive(Clone)]
pub enum Value {
    NumberValue(f64),
    BoolValue(bool),
    StringValue(String),
    LambdaValue(Argc, ApplyStartDBI, Vec<Expr>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberValue(v) => write!(f, "{} :: Number", v),
            BoolValue(v) => write!(f, "{} :: Bool", v),
            StringValue(v) => write!(f, "{} :: String", v),
            LambdaValue(argc, dbi, body) => {
                let gen = FsCodeGenerator::new();
                let code =
                    gen.partial_codegen_expr(AtomExpr(AtomLambda(*argc, *dbi, body.clone())));
                write!(f, "{}", code)
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
