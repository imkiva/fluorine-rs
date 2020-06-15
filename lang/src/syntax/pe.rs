use crate::syntax::tree::{
    Atom::{AtomId, AtomLambda, AtomLit},
    Expr::{ApplyExpr, AtomExpr, BinaryExpr, UnaryExpr, DBI},
    Lit::{LitBool, LitNumber},
    ProgramItem::{DeclItem, ExprItem},
    *,
};

pub trait PEContext {
    fn try_resolve_constant(&self, name: &str) -> Option<Expr>;
}

pub trait PartialEval
where
    Self: std::marker::Sized,
{
    type Output;

    fn partial_eval(self) -> Self::Output {
        self.partial_eval_with(None)
    }

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output;
}

impl<T: PartialEval<Output = T>> PartialEval for Box<T> {
    type Output = Box<T>;

    fn partial_eval_with(self: Self, ctx: Option<&dyn PEContext>) -> Self::Output {
        Box::new((*self).partial_eval_with(ctx))
    }
}

impl<T: PartialEval<Output = T>> PartialEval for Vec<T> {
    type Output = Vec<T>;

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output {
        self.into_iter().map(|t| t.partial_eval_with(ctx)).collect()
    }
}

impl PartialEval for ProgramItem {
    type Output = ProgramItem;

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output {
        match self {
            ExprItem(expr) => ExprItem(expr.partial_eval_with(ctx)),
            DeclItem(decl) => DeclItem(decl.partial_eval_with(ctx)),
        }
    }
}

impl PartialEval for Decl {
    type Output = Decl;

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output {
        match self {
            Decl::LetDecl(name, expr) => Decl::LetDecl(name, expr.partial_eval_with(ctx)),
        }
    }
}

impl PartialEval for Expr {
    type Output = Expr;

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output {
        match self {
            AtomExpr(atom) => atom.partial_eval_with(ctx),

            UnaryExpr(op, operand) => fold_unary(op, *operand.partial_eval_with(ctx), ctx),

            BinaryExpr(op, lhs, rhs) => fold_binary(
                op,
                *lhs.partial_eval_with(ctx),
                *rhs.partial_eval_with(ctx),
                ctx,
            ),

            ApplyExpr(f, a) => {
                fold_apply(*f.partial_eval_with(ctx), *a.partial_eval_with(ctx), ctx)
            }

            _ => self,
        }
    }
}

impl PartialEval for Atom {
    type Output = Expr;

    fn partial_eval_with(self, ctx: Option<&dyn PEContext>) -> Self::Output {
        match &self {
            AtomId(id) => match ctx.map(|c| c.try_resolve_constant(id)) {
                Some(Some(constant)) => constant,
                _ => AtomExpr(self),
            },
            _ => AtomExpr(self),
        }
    }
}

fn fold_unary(op: String, operand: Expr, _: Option<&dyn PEContext>) -> Expr {
    match (op.as_str(), &operand) {
        ("!", AtomExpr(AtomLit(LitBool(b)))) => AtomExpr(AtomLit(LitBool(!*b))),
        _ => UnaryExpr(op, Box::new(operand)),
    }
}

fn fold_binary(op: String, lhs: Expr, rhs: Expr, _: Option<&dyn PEContext>) -> Expr {
    match (op.as_str(), &lhs, &rhs) {
        ("&&", AtomExpr(AtomLit(LitBool(false))), _) => AtomExpr(AtomLit(LitBool(false))),
        ("&&", AtomExpr(AtomLit(LitBool(true))), _) => rhs,
        ("||", AtomExpr(AtomLit(LitBool(true))), _) => AtomExpr(AtomLit(LitBool(true))),
        ("||", AtomExpr(AtomLit(LitBool(false))), _) => rhs,

        ("&&", _, AtomExpr(AtomLit(LitBool(false)))) => AtomExpr(AtomLit(LitBool(false))),
        ("&&", _, AtomExpr(AtomLit(LitBool(true)))) => lhs,
        ("||", _, AtomExpr(AtomLit(LitBool(true)))) => AtomExpr(AtomLit(LitBool(true))),
        ("||", _, AtomExpr(AtomLit(LitBool(false)))) => lhs,

        ("+", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(a + b)))
        }
        ("-", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(a - b)))
        }
        ("*", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(a * b)))
        }
        ("/", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(a / b)))
        }
        ("%", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(a % b)))
        }
        ("^", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) => {
            AtomExpr(AtomLit(LitNumber(f64::powf(*a, *b))))
        }

        ("==", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a == *b))),
        ("!=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a != *b))),
        (">=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a >= *b))),
        (">", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a > *b))),
        ("<=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a <= *b))),
        ("<", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) => AtomExpr(AtomLit(LitBool(*a < *b))),

        _ => BinaryExpr(op, Box::new(lhs), Box::new(rhs)),
    }
}

fn fold_apply(f: Expr, a: Expr, ctx: Option<&dyn PEContext>) -> Expr {
    match f {
        AtomExpr(AtomLambda(ref argc, ref dbi, ref body)) if *dbi < *argc => {
            // the lambda still accepts argument
            let mut new_body: Vec<Expr> = body
                .into_iter()
                .map(|expr| subst(*dbi, expr.clone(), &a, ctx))
                .collect();

            if *dbi == *argc - 1 && new_body.len() == 1 {
                // 1. full applied after subst
                // 2. the lambda has only one expr in the body
                // so we can inline it!
                new_body.pop().unwrap().partial_eval_with(ctx)
            } else {
                AtomExpr(AtomLambda(*argc, *dbi + 1, new_body))
            }
        }

        AtomExpr(AtomLambda(_, _, _)) => ApplyExpr(Box::new(f), Box::new(a)),

        _ => ApplyExpr(Box::new(f), Box::new(a)),
    }
}

/// This is a specialized version of Subst::subst
/// Consider reuse the standard version in the future.
fn subst(dbi: i32, expr: Expr, replacement: &Expr, ctx: Option<&dyn PEContext>) -> Expr {
    match &expr {
        DBI(i) if dbi == *i => replacement.clone(),
        DBI(_) => expr,

        UnaryExpr(op, unary) => UnaryExpr(
            op.clone(),
            Box::new(subst(dbi, *unary.clone(), replacement, ctx).partial_eval_with(ctx)),
        ),

        BinaryExpr(op, lhs, rhs) => BinaryExpr(
            op.clone(),
            Box::new(subst(dbi, *lhs.clone(), replacement, ctx).partial_eval_with(ctx)),
            Box::new(subst(dbi, *rhs.clone(), replacement, ctx).partial_eval_with(ctx)),
        ),

        AtomExpr(AtomLambda(nested_argc, nested_dbi, nested_body)) => {
            let new_body: Vec<Expr> = nested_body
                .into_iter()
                .map(|ret_expr| {
                    subst(nested_argc + dbi, ret_expr.clone(), replacement, ctx)
                        .partial_eval_with(ctx)
                })
                .collect();
            AtomExpr(AtomLambda(*nested_argc, *nested_dbi, new_body))
        }

        ApplyExpr(f, arg) => fold_apply(
            subst(dbi, *f.clone(), replacement, ctx).partial_eval_with(ctx),
            subst(dbi, *arg.clone(), replacement, ctx).partial_eval_with(ctx),
            ctx,
        ),

        // TODO: partial evaluation MatchExpr
        _ => expr,
    }
}
