use std::collections::{HashMap, VecDeque};

use crate::{
    ffi::FFIClosure,
    runtime::{
        builtins::Builtins,
        pattern::Matcher,
        subst::Subst,
        Context, EnumType, RuntimeError,
        RuntimeError::{
            AmbiguousMember, ArgSelfTypeNotAllowed, ArgTypeMismatch, GenericNotSatisfied, NoMember,
            NonExhaustive, NotApplicable, StackUnderflow, TypeNotFound, VariableNotFound,
        },
        Scope, TraitImpl, TraitType, Type, Value,
        Value::{
            BoolValue, EnumCtor, EnumValue, ForeignLambda, LambdaValue, NumberValue, StringValue,
            UnitValue,
        },
    },
    syntax::{
        pe::{PEContext, PartialEval},
        tree::{
            Atom,
            Atom::{AtomId, AtomLambda, AtomLit, AtomRawLambda},
            Constraint, Decl,
            Decl::{EnumDecl, ImplDecl, LetDecl, TraitDecl},
            Expr,
            Expr::{ApplyExpr, AtomExpr, BinaryExpr, MatchExpr, MemberExpr, UnaryExpr, Unit, DBI},
            GenericParam, Ident, Lit,
            Lit::{LitBool, LitNumber, LitString},
            MatchCase, Param, ParseType, Program, ProgramItem,
            ProgramItem::{DeclItem, ExprItem},
            TraitFn,
        },
    },
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
            EnumDecl(generic, name, variants) => {
                let ty = EnumType {
                    name: name.clone(),
                    generic,
                    variants: variants.clone(),
                };

                // make each variant a constructor lambda
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
                ctx.put_enum(name, ty)?;
                Ok(UnitValue)
            }
            TraitDecl(name, fns) => {
                ctx.put_trait(name, fns)?;
                Ok(UnitValue)
            }
            ImplDecl(generic, tr, ty, fns) => {
                let fns = fns
                    .into_iter()
                    .map(|decl| match decl {
                        Decl::LetDecl(id, lambda) => (id, lambda.eval_into(ctx).unwrap()),
                        _ => unreachable!("not a trait fn"),
                    })
                    .collect();
                // TODO: check if ty is a generic param
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
            ApplyExpr(f, a) => eval_apply(ctx, *f, *a),
            MatchExpr(expr, cases) => eval_match(ctx, *expr, cases),
            MemberExpr(lhs, id) => eval_member(ctx, *lhs, id),

            DBI(_) => unreachable!("dangling dbi"),
            UnaryExpr(_, _) => unreachable!("Desugar bug"),
            BinaryExpr(_, _, _) => unreachable!("Desugar bug"),
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

    fn put_enum(&mut self, name: String, e: EnumType) -> Result<(), RuntimeError> {
        self.stack
            .front_mut()
            .ok_or(StackUnderflow)?
            .enums
            .insert(name, e);
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

    pub fn impl_trait(
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

    fn get_impl_for(&self, ty: &Type) -> Result<Option<&Vec<TraitImpl>>, RuntimeError> {
        match self.stack.front().ok_or(StackUnderflow)?.impls.get(ty) {
            Some(impls) => Ok(Some(impls)),
            _ => Ok(None),
        }
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
            Some(e) => Ok(e.clone()),
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

fn check_lambda_param(
    ctx: &mut Context,
    generic: &Vec<GenericParam>,
    index: usize,
    expected: Option<ParseType>,
    arg: &Expr,
) -> Result<(), RuntimeError> {
    match expected {
        // if provided type, then we check
        Some(ty) => match ty {
            // we don't allow Self in lambdas, instead
            // Self are only allowed in trait fns.
            ParseType::SelfType => Err(ArgSelfTypeNotAllowed(index)),
            ParseType::OtherType(name) => {
                match find_generic(generic, name.clone()) {
                    Some(generic) => {
                        // if name is a generic param, we should check constraints
                        check_generic(ctx, index, generic, arg)
                    }
                    _ => {
                        // otherwise we resolve it as concrete type
                        check_concrete(ctx, index, ctx.resolve_type(name)?, arg)
                    }
                }
            }
        },

        // otherwise, we accept
        _ => Ok(()),
    }
}

fn find_generic(generic: &Vec<GenericParam>, name: String) -> Option<GenericParam> {
    generic
        .iter()
        .position(|p| p.name == name)
        .map(|idx| generic[idx].clone())
}

fn check_concrete(
    ctx: &mut Context,
    index: usize,
    expected: Type,
    arg: &Expr,
) -> Result<(), RuntimeError> {
    // TODO: type inference instead of eval
    let got = arg.clone().eval_into(ctx)?.get_type();
    if expected != got {
        Err(ArgTypeMismatch(index, expected, got))
    } else {
        Ok(())
    }
}

fn check_generic(
    ctx: &mut Context,
    index: usize,
    generic: GenericParam,
    arg: &Expr,
) -> Result<(), RuntimeError> {
    // no constraints, just accept
    if generic.constraints.is_empty() {
        return Ok(());
    }

    // TODO: type inference instead of eval
    let got = arg.clone().eval_into(ctx)?.get_type();
    let impls = match ctx.get_impl_for(&got)? {
        Some(impls) => impls,
        _ => return Err(GenericNotSatisfied(index, generic, got)),
    };

    let result = generic
        .constraints
        .iter()
        .map(|c| satisfy_constraint(impls, c))
        .all(|result| result);

    match result {
        true => Ok(()),
        _ => Err(GenericNotSatisfied(index, generic, got)),
    }
}

fn satisfy_constraint(impls: &Vec<TraitImpl>, constraint: &Constraint) -> bool {
    let name = match constraint {
        Constraint::MustImpl(id) => id,
    };

    impls
        .iter()
        .position(|i| i.tr.name.as_str() == name)
        .is_some()
}

fn eval_apply(ctx: &mut Context, f: Expr, arg: Expr) -> Result<Value, RuntimeError> {
    // We should eval the arg into a normalized form as we are binding
    // the arg to the DBI(dbi) expr
    let arg = arg.partial_eval_with(Some(ctx));

    match f.eval_into(ctx)? {
        LambdaValue(param, dbi, body) => {
            let argc = param.len();
            debug_assert_ne!(argc, dbi);
            check_lambda_param(ctx, &vec![], dbi, param[dbi].ty.clone(), &arg)?;

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
            let argc = variant.field_types.len();
            let dbi = fields.len();
            debug_assert_ne!(argc, dbi);
            check_lambda_param(
                ctx,
                &ty.generic,
                dbi,
                Some(ParseType::OtherType(variant.field_types[dbi].clone())),
                &arg,
            )?;

            fields.push(arg.eval_into(ctx)?);
            if dbi + 1 == argc {
                Ok(EnumValue(ty, variant, fields))
            } else {
                Ok(EnumCtor(ty, variant, fields))
            }
        }

        ForeignLambda(closure, mut argv) => {
            let argc = closure.param.len();
            let dbi = argv.len();
            debug_assert_ne!(argc, dbi);

            // We don't check parameter type of ffi lambda,
            // because they are checked when the ffi call happens.

            argv.push_back(arg.eval_into(ctx)?);
            if dbi + 1 == argc {
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

fn eval_member(ctx: &mut Context, lhs: Expr, id: Ident) -> Result<Value, RuntimeError> {
    // TODO: type inference instead of eval
    let lhs = lhs.partial_eval_with(Some(ctx));
    let lhs_value = lhs.clone().eval_into(ctx)?;
    let ty = lhs_value.get_type();

    if let Type::LambdaType(_) = &ty {
        // we do not support impl trait for lambdas
        return Err(NoMember(id, ty));
    }

    let impls = ctx
        .get_impl_for(&ty)?
        .ok_or(NoMember(id.clone(), ty.clone()))?;
    let found = impls
        .iter()
        .filter_map(|it| it.impls.get(id.as_str()))
        .collect::<Vec<_>>();

    match found.len() {
        0 => return Err(NoMember(id, ty)),
        1 => match (*found.first().unwrap()).clone() {
            LambdaValue(param, dbi, body) => eval_member_lambda(ctx, lhs, param, dbi, body),
            ForeignLambda(ffi, value) => eval_member_foreign(ctx, lhs_value, ffi, value),
            _ => unreachable!("not a valid trait fn implementation"),
        },
        _ => Err(AmbiguousMember(id)),
    }
}

fn eval_member_lambda(
    ctx: &mut Context,
    lhs: Expr,
    param: Vec<Param>,
    dbi: usize,
    body: Vec<Expr>,
) -> Result<Value, RuntimeError> {
    let argc = param.len();
    debug_assert_ne!(dbi, argc);
    debug_assert_eq!(dbi, 0);

    match (param[0].id.as_str(), &param[0].ty) {
        ("self", Some(ParseType::SelfType)) => {
            let body = body.subst(dbi, &lhs).partial_eval_with(Some(ctx));
            Ok(LambdaValue(param, dbi + 1, body))
        }
        _ => unimplemented!("static trait fn not supported"),
    }
}

fn eval_member_foreign(
    _ctx: &mut Context,
    lhs: Value,
    ffi: FFIClosure,
    mut value: VecDeque<Value>,
) -> Result<Value, RuntimeError> {
    let dbi = value.len();
    let argc = ffi.param.len();
    debug_assert_ne!(dbi, argc);
    debug_assert_eq!(dbi, 0);

    match (ffi.param[0].id.as_str(), &ffi.param[0].ty) {
        ("self", Some(ParseType::SelfType)) => {
            value.push_back(lhs);
            Ok(ForeignLambda(ffi, value))
        }
        _ => unimplemented!("static trait fn not supported"),
    }
}
