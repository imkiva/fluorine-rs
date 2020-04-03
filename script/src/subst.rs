use crate::tree::Expr;
use crate::tree::Expr::{DBI, UnaryExpr, BinaryExpr, AtomExpr, ApplyExpr};
use crate::tree::Atom::AtomLambda;

pub struct Substitution;

impl Substitution {
    pub fn subst(dbi: i32, expr: Expr, replacement: &Expr) -> Expr {
        match &expr {
            DBI(i) if dbi == *i => replacement.clone(),
            DBI(_) => expr,

            UnaryExpr(op, unary) =>
                UnaryExpr(op.clone(), Box::new(Substitution::subst(dbi, *unary.clone(), replacement))),

            BinaryExpr(op, lhs, rhs) =>
                BinaryExpr(op.clone(), Box::new(Substitution::subst(dbi, *lhs.clone(), replacement)),
                           Box::new(Substitution::subst(dbi, *rhs.clone(), replacement))),

            AtomExpr(AtomLambda(nested_argc, nested_dbi, nested_body)) => {
                let new_body: Vec<Expr> = nested_body.into_iter()
                    .map(|expr| Substitution::subst(nested_argc + dbi, expr.clone(), replacement))
                    .collect();
                AtomExpr(AtomLambda(*nested_argc, *nested_dbi, new_body))
            }

            ApplyExpr(f, arg) =>
                ApplyExpr(Box::new(Substitution::subst(dbi, *f.clone(), replacement)),
                          Box::new(Substitution::subst(dbi, *arg.clone(), replacement))),

            _ => expr,
        }
    }
}
