use crate::{
    runtime::{
        subst::Subst,
        RuntimeError::{NonExhaustive, NotApplicable, StackUnderflow, VariableNotFound},
        Value::{BoolValue, LambdaValue, NumberValue, StringValue},
    },
    syntax::tree::{
        Atom,
        Atom::{AtomId, AtomLambda, AtomLit, AtomRawLambda},
        Decl,
        Decl::LetDecl,
        Expr,
        Expr::{ApplyExpr, AtomExpr, BinaryExpr, MatchExpr, UnaryExpr, Unit, DBI},
        Ident, Lit,
        Lit::{LitBool, LitNumber, LitString},
        MatchCase, Program, ProgramItem,
        ProgramItem::{DeclItem, ExprItem},
    },
};

use crate::{
    ffi::FFIClosure,
    runtime::{
        builtins::Builtins,
        pattern::Matcher,
        Context, EnumType, RuntimeError,
        RuntimeError::{TypeMismatch, TypeNotFound},
        Scope, TraitImpl, TraitType, Type, Value,
        Value::{EnumCtor, EnumValue, ForeignLambda, UnitValue},
    },
    syntax::{
        pe::{PEContext, PartialEval},
        tree::{
            Decl::{EnumDecl, ImplDecl, TraitDecl},
            EnumVariant,
            Expr::MemberExpr,
            TraitFn,
        },
    },
};
use std::{
    collections::{HashMap, VecDeque},
    ops::Not,
};

pub(crate) trait Eval {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError>;
}

impl<T: Eval> Eval for Box<T> {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        (*self).eval_into(ctx)
    }
}

impl<T: Eval> Eval for Vec<T> {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        let mut results = Vec::new();
        for expr in self {
            results.push(expr.eval_into(ctx)?);
        }

        if let Some(r) = results.pop() {
            Ok(r)
        } else {
            Ok(UnitValue)
        }
    }
}

impl Eval for ProgramItem {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        match self {
            ExprItem(expr) => expr.eval_into(ctx),
            DeclItem(decl) => decl.eval_into(ctx),
        }
    }
}

impl Eval for Decl {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        match self {
            LetDecl(name, expr) => {
                let value = expr.eval_into(ctx)?;
                ctx.put_var(name, value)?;
                Ok(UnitValue)
            }
            EnumDecl(name, variants) => {
                let ty = EnumType {
                    name: name.clone(),
                    variants: variants.clone(),
                };
                for variant in &variants {
                    match variant.field_types.len() {
                        0 => ctx.put_var(
                            variant.name.clone(),
                            EnumValue(ty.clone(), variant.clone(), Vec::new()),
                        )?,
                        _ => ctx.put_var(
                            variant.name.clone(),
                            EnumCtor(
                                ty.clone(),
                                variant.clone(),
                                Vec::with_capacity(variant.field_types.len()),
                            ),
                        )?,
                    };
                }
                ctx.put_enum(name, variants)?;
                Ok(UnitValue)
            }
            TraitDecl(name, fns) => {
                ctx.put_trait(name, fns)?;
                Ok(UnitValue)
            }
            ImplDecl(tr, ty, fns) => {
                let fns = fns
                    .into_iter()
                    .map(|decl| match decl {
                        Decl::LetDecl(id, lambda) => (id, lambda.eval_into(ctx).unwrap()),
                        _ => unreachable!("not a trait fn"),
                    })
                    .collect();
                let ty = ctx.resolve_type(ty)?;
                ctx.impl_trait(tr, ty, fns)?;
                Ok(UnitValue)
            }
        }
    }
}

impl Eval for Expr {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        match self {
            Unit => Ok(UnitValue),
            AtomExpr(atom) => atom.eval_into(ctx),
            UnaryExpr(op, operand) => match op.as_str() {
                "!" => {
                    let val = operand.eval_into(ctx)?;
                    Ok(val.not()?)
                }
                _ => unreachable!("Unexpected unary operator"),
            },

            BinaryExpr(op, lhs, rhs) => {
                let l = lhs.eval_into(ctx)?;
                let r = rhs.eval_into(ctx)?;

                match op.as_str() {
                    "+" => Ok((l + r)?),
                    "-" => Ok((l - r)?),
                    "*" => Ok((l * r)?),
                    "/" => Ok((l / r)?),
                    "%" => Ok((l % r)?),
                    "&&" => Ok(l.logical_and(r)?),
                    "||" => Ok(l.logical_or(r)?),
                    "^" => Ok((l.pow(r))?),
                    "==" => Ok(BoolValue(l == r)),
                    "!=" => Ok(BoolValue(l != r)),
                    ">" => Ok(BoolValue(l > r)),
                    ">=" => Ok(BoolValue(l >= r)),
                    "<" => Ok(BoolValue(l < r)),
                    "<=" => Ok(BoolValue(l <= r)),
                    _ => unreachable!("Unexpected binary operator"),
                }
            }

            ApplyExpr(f, a) => eval_apply(ctx, *f, *a),

            MatchExpr(expr, cases) => eval_match(ctx, *expr, cases),

            MemberExpr(lhs, id) => unimplemented!("// TODO"),

            DBI(_) => unreachable!("dangling dbi"),
        }
    }
}

impl Eval for Atom {
    fn eval_into(self, ctx: &mut Context) -> Result<Value, RuntimeError> {
        match self {
            AtomLit(lit) => lit.eval_into(ctx),
            AtomId(id) => ctx.get_var(id.as_str()),
            AtomLambda(argc, dbi, body) => Ok(LambdaValue(argc, dbi, body)),
            AtomRawLambda(_, _) => unreachable!("dangling raw lambda"),
        }
    }
}

impl Eval for Lit {
    fn eval_into(self, _: &mut Context) -> Result<Value, RuntimeError> {
        match self {
            LitNumber(l) => Ok(NumberValue(l)),
            LitString(l) => Ok(StringValue(l)),
            LitBool(l) => Ok(BoolValue(l)),
        }
    }
}

impl std::ops::Add for Value {
    type Output = Result<Value, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l + r)),
            (StringValue(l), StringValue(r)) => Ok(StringValue(l + r.as_str())),
            _ => Err(TypeMismatch),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Result<Value, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l - r)),
            _ => Err(TypeMismatch),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Value, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l * r)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Value, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l / r)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl std::ops::Rem for Value {
    type Output = Result<Value, RuntimeError>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l % r)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Result<Value, RuntimeError>;

    fn not(self) -> Self::Output {
        match self {
            BoolValue(b) => Ok(BoolValue(!b)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

trait Pow {
    type Output;
    fn pow(self, rhs: Self) -> Self::Output;
}

impl Pow for Value {
    type Output = Result<Value, RuntimeError>;

    fn pow(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumberValue(l), NumberValue(r)) => Ok(NumberValue(l.powf(r))),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

trait NoShortCircuitLogicalAnd {
    type Output;
    fn logical_and(self, rhs: Self) -> Self::Output;
}

trait NoShortCircuitLogicalOr {
    type Output;
    fn logical_or(self, rhs: Self) -> Self::Output;
}

impl NoShortCircuitLogicalAnd for Value {
    type Output = Result<Value, RuntimeError>;

    fn logical_and(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (BoolValue(l), BoolValue(r)) => Ok(BoolValue(l && r)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl NoShortCircuitLogicalOr for Value {
    type Output = Result<Value, RuntimeError>;

    fn logical_or(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (BoolValue(l), BoolValue(r)) => Ok(BoolValue(l || r)),
            _ => Err(RuntimeError::TypeMismatch),
        }
    }
}

impl Context {
    pub fn new() -> Context {
        let mut stack = VecDeque::new();
        stack.push_front(Scope::new());
        Context { stack }
    }

    pub fn load_builtins(&mut self) {
        Builtins::init(self)
    }

    pub fn source(&mut self, input: Program) -> Result<Value, RuntimeError> {
        input.eval_into(self)
    }

    fn put_var(&mut self, name: Ident, value: Value) -> Result<Option<Value>, RuntimeError> {
        Ok(self
            .stack
            .front_mut()
            .ok_or(StackUnderflow)?
            .vars
            .insert(name, value))
    }

    fn get_var(&self, name: &str) -> Result<Value, RuntimeError> {
        for scope in &self.stack {
            if let Some(v) = scope.vars.get(name) {
                return Ok(v.clone());
            }
        }

        Err(VariableNotFound(name.to_owned()))
    }

    fn put_enum(&mut self, name: String, variants: Vec<EnumVariant>) -> Result<(), RuntimeError> {
        self.stack
            .front_mut()
            .ok_or(StackUnderflow)?
            .enums
            .insert(name, variants);
        Ok(())
    }

    fn put_trait(&mut self, name: String, fns: Vec<TraitFn>) -> Result<(), RuntimeError> {
        self.stack
            .front_mut()
            .ok_or(StackUnderflow)?
            .traits
            .insert(name, fns);
        Ok(())
    }

    fn impl_trait(
        &mut self,
        tr: Ident,
        ty: Type,
        fns: HashMap<Ident, Value>,
    ) -> Result<(), RuntimeError> {
        let trait_impl = TraitImpl {
            tr: self.resolve_trait(tr)?,
            impls: fns,
        };

        let impls = &mut self.stack.front_mut().ok_or(StackUnderflow)?.impls;

        match impls.get_mut(&ty) {
            Some(impls) => impls.push(trait_impl),
            _ => {
                let _ = impls.insert(ty, vec![trait_impl]);
            }
        }

        Ok(())
    }

    fn resolve_type(&self, name: String) -> Result<Type, RuntimeError> {
        match name.as_str() {
            "Unit" => Ok(Type::UnitType),
            "Number" => Ok(Type::NumberType),
            "Bool" => Ok(Type::BoolType),
            "String" => Ok(Type::StringType),
            _ => match self.resolve_enum(name.clone()) {
                Ok(e) => Ok(Type::EnumType(e)),
                _ => self.resolve_trait(name).map(Type::TraitType),
            },
        }
    }

    fn resolve_trait(&self, name: String) -> Result<TraitType, RuntimeError> {
        match self
            .stack
            .front()
            .ok_or(StackUnderflow)?
            .traits
            .get(name.as_str())
        {
            Some(fns) => Ok(TraitType {
                name: name.to_string(),
                fns: fns
                    .clone()
                    .into_iter()
                    .map(|f| (f.name.clone(), f))
                    .collect(),
            }),
            _ => Err(TypeNotFound(name)),
        }
    }

    fn resolve_enum(&self, name: String) -> Result<EnumType, RuntimeError> {
        match self
            .stack
            .front()
            .ok_or(StackUnderflow)?
            .enums
            .get(name.as_str())
        {
            Some(variants) => Ok(EnumType {
                name,
                variants: variants.clone(),
            }),
            _ => Err(TypeNotFound(name)),
        }
    }

    pub fn ffi(
        &mut self,
        name: String,
        closure: FFIClosure,
    ) -> Result<Option<Value>, RuntimeError> {
        let argc = closure.param.len();
        self.put_var(
            name,
            Value::ForeignLambda(closure, VecDeque::with_capacity(argc)),
        )
    }

    fn new_scope(&mut self) {
        self.stack.push_front(Scope::new())
    }

    fn pop_scope(&mut self) -> Result<(), RuntimeError> {
        self.stack.pop_front().ok_or(StackUnderflow).map(|_| ())
    }
}

impl PEContext for Context {
    fn try_resolve_constant(&self, name: &str) -> Option<Expr> {
        match self.get_var(name) {
            Ok(NumberValue(n)) => Some(AtomExpr(AtomLit(LitNumber(n)))),
            Ok(BoolValue(b)) => Some(AtomExpr(AtomLit(LitBool(b)))),
            Ok(StringValue(s)) => Some(AtomExpr(AtomLit(LitString(s)))),
            Ok(LambdaValue(argc, dbi, body)) => Some(AtomExpr(AtomLambda(argc, dbi, body))),
            Ok(UnitValue) => Some(Unit),
            _ => None,
        }
    }
}

impl Scope {
    fn new() -> Scope {
        Scope {
            vars: Default::default(),
            enums: Default::default(),
            traits: Default::default(),
            impls: Default::default(),
        }
    }
}

fn eval_apply(ctx: &mut Context, f: Expr, arg: Expr) -> Result<Value, RuntimeError> {
    // We should eval the arg into a normalized form as we are binding
    // the arg to the DBI(dbi) expr
    let arg = arg.partial_eval_with(Some(ctx));

    match f.eval_into(ctx)? {
        LambdaValue(param, dbi, body) => {
            let argc = param.len();
            debug_assert_ne!(argc, dbi);
            let new_body = body.subst(dbi, &arg).partial_eval_with(Some(ctx));
            if dbi + 1 == argc {
                ctx.new_scope();
                let result = match new_body.eval_into(ctx) {
                    Ok(value) => value,
                    err => {
                        ctx.pop_scope()?;
                        return err;
                    }
                };
                ctx.pop_scope()?;
                Ok(result)
            } else {
                Ok(LambdaValue(param, dbi + 1, new_body))
            }
        }

        // We only handle enum constructors that has fields,
        // constructors with no fields are handled in Decl::eval_into()
        EnumCtor(ty, variant, mut fields) => {
            debug_assert_ne!(variant.field_types.len(), fields.len());
            fields.push(arg.eval_into(ctx)?);
            if fields.len() == variant.field_types.len() {
                Ok(EnumValue(ty, variant, fields))
            } else {
                Ok(EnumCtor(ty, variant, fields))
            }
        }

        ForeignLambda(closure, mut argv) => {
            let argc = closure.param.len();
            debug_assert_ne!(argc, argv.len());
            argv.push_back(arg.eval_into(ctx)?);
            if argv.len() == argc {
                (closure.closure)(argv).map_err(|e| e.into())
            } else {
                Ok(ForeignLambda(closure, argv))
            }
        }

        _ => Err(NotApplicable),
    }
}

fn eval_match(
    ctx: &mut Context,
    matchee: Expr,
    cases: Vec<MatchCase>,
) -> Result<Value, RuntimeError> {
    let value = matchee.eval_into(ctx)?;

    match cases.try_match(&value) {
        Some((records, result)) => {
            // We cannot just override the scope neither create a new scope,
            // instead we should backup the vars that will be overwritten
            // and restore them when match ends.
            let mut backup = HashMap::with_capacity(records.len());
            for (name, value) in records {
                backup.insert(name.clone(), ctx.put_var(name, value)?);
            }
            let result = result.eval_into(ctx);
            for (name, value) in backup {
                if let Some(value) = value {
                    ctx.put_var(name, value)?;
                }
            }
            result
        }
        _ => Err(NonExhaustive),
    }
}
