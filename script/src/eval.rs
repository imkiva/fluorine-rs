use std::collections::{VecDeque, HashMap};
use crate::tree::{Atom, Program, Expr, Decl, ProgramItem, Lit, Name, Argc, ApplyStartDBI};
use crate::tree::ProgramItem::{ExprItem, DeclItem};
use crate::tree::Decl::LetDecl;
use crate::tree::Expr::{AtomExpr, DBI, UnaryExpr, BinaryExpr, ApplyExpr};
use crate::eval::RuntimeError::{VariableNotFound, StackUnderflow, BottomValue, DanglingDBI, DanglingRawLambda};
use crate::tree::Atom::{AtomLit, AtomId, AtomLambda, AtomRawLambda};
use crate::tree::Lit::{LitNumber, LitString, LitBool};
use crate::eval::Value::{NumberValue, StringValue, BoolValue, LambdaValue};

pub enum RuntimeError {
    DanglingDBI,
    DanglingRawLambda,
    StackUnderflow,
    VariableNotFound(String),
    BottomValue,
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
                let value = expr.eval_into(ctx)?.ok_or(BottomValue)?;
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
            UnaryExpr(_, _) => Ok(None),
            BinaryExpr(_, _, _) => Ok(None),
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
