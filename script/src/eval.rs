use crate::tree::{Atom, Program, Expr, Decl, ProgramItem, Lit, Name, Argc, ApplyStartDBI};
use crate::tree::ProgramItem::{ExprItem, DeclItem};
use crate::tree::Decl::LetDecl;
use crate::tree::Expr::{AtomExpr, DBI, UnaryExpr, BinaryExpr, ApplyExpr};
use crate::eval::RuntimeError::{VariableNotFound, StackUnderflow, BottomTypedValue, DanglingDBI, DanglingRawLambda, NotApplicable};
use crate::tree::Atom::{AtomLit, AtomId, AtomLambda, AtomRawLambda};
use crate::tree::Lit::{LitNumber, LitString, LitBool};
use crate::eval::Value::{NumberValue, StringValue, BoolValue, LambdaValue};
use crate::codegen::fs::FsCodeGenerator;
use crate::codegen::PartialCodeGenerator;
use crate::subst::Subst;

use std::collections::{VecDeque, HashMap};
use std::ops::Not;
use std::cmp::Ordering;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum RuntimeError {
    DanglingDBI,
    DanglingRawLambda,
    StackUnderflow,
    VariableNotFound(String),
    NotApplicable,
    BottomTypedValue,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DanglingDBI => write!(f, "InternalError: detected dangling DBI outside lambda"),
            DanglingRawLambda => write!(f, "InternalError: detected unresolved lambda"),
            StackUnderflow => write!(f, "RuntimeError: stack underflow"),
            VariableNotFound(id) => write!(f, "NameError: variable '{}' not found", id),
            BottomTypedValue => write!(f, "TypeError: try to produce bottom typed value"),
            NotApplicable => write!(f, "TypeError: not a lambda"),
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
            LambdaValue(argc, dbi, body) => {
                let gen = FsCodeGenerator::new();
                let code = gen.partial_codegen_expr(AtomExpr(AtomLambda(
                    *argc, *dbi, body.clone(),
                )));
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
        let mut results = Vec::new();
        for expr in self {
            results.push(expr.eval_into(ctx)?);
        }

        if let Some(r) = results.pop() {
            Ok(r)
        } else {
            Ok(None)
        }
    }
}

impl Eval for ProgramItem {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            ExprItem(expr) => expr.eval_into(ctx),
            DeclItem(decl) => decl.eval_into(ctx),
        }
    }
}

impl Eval for Decl {
    fn eval_into(self, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
        match self {
            LetDecl(name, expr) => {
                let value = expr.eval_into(ctx)?.ok_or(BottomTypedValue)?;
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
                    let val = operand.eval_into(ctx)?.ok_or(BottomTypedValue)?;
                    Ok(val.not())
                }
                _ => unreachable!("Unexpected unary operator"),
            }

            BinaryExpr(op, lhs, rhs) => {
                let l = lhs.eval_into(ctx)?.ok_or(BottomTypedValue)?;
                // logical operators should be short-circuit
                match op.as_str() {
                    "&&" => return Ok(None),
                    "||" => return Ok(None),
                    _ => (),
                };

                let r = rhs.eval_into(ctx)?.ok_or(BottomTypedValue)?;
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

            ApplyExpr(f, a) =>
                eval_apply(ctx, *f, *a),

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

fn eval_apply(ctx: &mut Context, f: Expr, a: Expr) -> Result<Option<Value>, RuntimeError> {
    match f.eval_into(ctx)? {
        Some(LambdaValue(argc, dbi, body)) => {
            debug_assert_ne!(argc, dbi);
            let new_body = body.subst(dbi, &a);
            if dbi + 1 == argc {
                ctx.new_scope();
                let result = new_body.eval_into(ctx)?;
                ctx.pop_scope()?;
                Ok(result)
            } else {
                Ok(Some(LambdaValue(argc, dbi + 1, new_body)))
            }
        }
        Some(_) => Err(NotApplicable),
        None => Err(BottomTypedValue),
    }
}
