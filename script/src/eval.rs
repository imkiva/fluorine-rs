use std::collections::{VecDeque, HashMap};
use crate::tree::{Atom, Program, Expr, Decl, ProgramItem, Lit, Name, Argc, ApplyStartDBI};
use crate::tree::ProgramItem::{ExprItem, DeclItem};
use crate::tree::Decl::LetDecl;
use crate::tree::Expr::{AtomExpr, DBI, UnaryExpr, BinaryExpr, ApplyExpr};
use crate::eval::RuntimeError::{VariableNotFound, StackUnderflow, BottomType, DanglingDBI, DanglingRawLambda};
use crate::tree::Atom::{AtomLit, AtomId, AtomLambda, AtomRawLambda};
use crate::tree::Lit::{LitNumber, LitString, LitBool};
use crate::eval::Value::{NumberValue, StringValue, BoolValue, LambdaValue};
use std::ops::Not;
use std::cmp::Ordering;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum RuntimeError {
    DanglingDBI,
    DanglingRawLambda,
    StackUnderflow,
    VariableNotFound(String),
    BottomType,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DanglingDBI => write!(f, "InternalError: detected dangling DBI outside lambda"),
            DanglingRawLambda => write!(f, "InternalError: detected unresolved lambda"),
            StackUnderflow => write!(f, "RuntimeError: stack underflow"),
            VariableNotFound(id) => write!(f, "NameError: variable {} not found", id),
            BottomType => write!(f, "RuntimeError: try to produce bottom typed value"),
        }
    }
}

pub struct Context {
    stack: VecDeque<Scope>,
}

struct Scope {
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
            LambdaValue(argc, dbi, _) => {
                let takes = argc - dbi;
                write!(f, "<lambda-with-{}-{}>", takes,
                       if takes == 1 { "arg" } else { "args" })
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

trait Eval {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError>;
}

impl<T: Eval> Eval for Box<T> {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        (*self).eval_into(ctx)
    }
}

impl<T: Eval> Eval for Vec<T> {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        for expr in self {
            expr.eval_into(ctx)?;
        }

        Ok(None)
    }
}

impl Eval for ProgramItem {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            ExprItem(expr) => expr.eval_into(ctx),
            DeclItem(decl) => decl.eval_into(ctx),
            _ => Ok(None),
        }
    }
}

impl Eval for Decl {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            LetDecl(name, expr) => {
                let value = expr.eval_into(ctx)?.ok_or(BottomType)?;
                ctx.put_var(name, value)?;
                Ok(None)
            }
        }
    }
}

impl Eval for Expr {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            AtomExpr(atom) => atom.eval_into(ctx),
            UnaryExpr(op, operand) => match op.as_str() {
                "!" => {
                    let val = operand.eval_into(ctx)?.ok_or(BottomType)?;
                    Ok(val.not())
                }
                _ => unreachable!("Unexpected unary operator"),
            }

            BinaryExpr(op, lhs, rhs) => {
                let l = lhs.eval_into(ctx)?.ok_or(BottomType)?;
                // logical operators should be short-circuit
                match op.as_str() {
                    "&&" => return Ok(None),
                    "||" => return Ok(None),
                    _ => (),
                };

                let r = rhs.eval_into(ctx)?.ok_or(BottomType)?;
                match op.as_str() {
                    "+" => Ok(l + r),
                    "-" => Ok(l - r),
                    "*" => Ok(l * r),
                    "/" => Ok(l / r),
                    "%" => Ok(l % r),
                    "^" => Ok(l.pow(r)),
                    "==" => Ok(Some(BoolValue(l == r))),
                    "!=" => Ok(Some(BoolValue(l != r))),
                    ">" => Ok(Some(BoolValue(l > r))),
                    ">=" => Ok(Some(BoolValue(l >= r))),
                    "<" => Ok(Some(BoolValue(l < r))),
                    "<=" => Ok(Some(BoolValue(l <= r))),
                    _ => unreachable!("Unexpected binary operator"),
                }
            }

            ApplyExpr(_, _) => Ok(None),

            DBI(_) => Err(DanglingDBI),
            _ => unreachable!("Internal Error"),
        }
    }
}

impl Eval for Atom {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            AtomLit(lit) => lit.eval_into(ctx),
            AtomId(id) => ctx.get_var(id).map(Some),
            AtomLambda(argc, dbi, body) => Ok(Some(LambdaValue(argc, dbi, body))),
            AtomRawLambda(_, _) => Err(DanglingRawLambda),
        }
    }
}

impl Eval for Lit {
    fn eval_into(self, _: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            LitNumber(l) => Ok(Some(NumberValue(l))),
            LitString(l) => Ok(Some(StringValue(l))),
            LitBool(l) => Ok(Some(BoolValue(l))),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Option<Value>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l + r)),
            (StringValue(l), StringValue(r)) => Some(StringValue(l + r.as_str())),
            _ => None,
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Option<Value>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l - r)),
            _ => None,
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Option<Value>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l * r)),
            _ => None,
        }
    }
}

impl std::ops::Div for Value {
    type Output = Option<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l / r)),
            _ => None,
        }
    }
}

impl std::ops::Rem for Value {
    type Output = Option<Value>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l % r)),
            _ => None,
        }
    }
}

impl std::ops::Not for Value {
    type Output = Option<Value>;

    fn not(self) -> Self::Output {
        match self {
            BoolValue(b) => Some(BoolValue(!b)),
            _ => None,
        }
    }
}

trait Pow {
    type Output;
    fn pow(self, rhs: Self) -> Self::Output;
}

impl Pow for Value {
    type Output = Option<Value>;

    fn pow(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Some(NumberValue(l.powf(r))),
            _ => None,
        }
    }
}

impl Context {
    pub fn new() -> Context {
        let mut stack = VecDeque::new();
        stack.push_front(Scope::new());
        Context { stack }
    }

    pub fn source(&mut self, input: Program) -> Result<Option<Value>, RuntimeError> {
        input.eval_into(self)
    }

    fn put_var(&mut self, name: Name, value: Value) -> Result<(), RuntimeError> {
        self.stack.front_mut()
            .ok_or(StackUnderflow)?
            .vars.insert(name, value);
        Ok(())
    }

    fn get_var(&mut self, name: Name) -> Result<Value, RuntimeError> {
        self.stack.front()
            .ok_or(StackUnderflow)?
            .vars.get(name.as_str())
            .map(|v| v.clone())
            .ok_or(VariableNotFound(name))
    }

    fn new_scope(&mut self) {
        self.stack.push_front(Scope::new())
    }

    fn pop_scope(&mut self) -> Result<(), RuntimeError> {
        self.stack.pop_front()
            .ok_or(StackUnderflow)
            .map(|_| ())
    }
}

impl Scope {
    fn new() -> Scope {
        Scope {
            vars: Default::default(),
        }
    }
}
