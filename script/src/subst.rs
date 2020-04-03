use crate::tree::Expr;
use crate::tree::Expr::{DBI, UnaryExpr, BinaryExpr, AtomExpr, ApplyExpr};
use crate::tree::Atom::AtomLambda;

pub struct Substitution;

trait Subst {
    type Output;
    fn subst(self: Self, dbi: i32, replacement: &Expr) -> Self::Output;
}

impl<T: Subst<Output=T>> Subst for Box<T> {
    type Output = Box<T>;

    fn subst(self: Self, dbi: i32, replacement: &Expr) -> Self::Output {
        let t = *self;
        Box::new(t.subst(dbi, replacement))
    }
}

impl<T: Subst<Output=T>> Subst for Vec<T> {
    type Output = Vec<T>;

    fn subst(self: Self, dbi: i32, replacement: &Expr) -> Self::Output {
        self.into_iter()
            .map(|expr| expr.subst(dbi, replacement))
            .collect()
    }
}

impl Subst for Expr {
    type Output = Expr;

    fn subst(self: Self, dbi: i32, replacement: &Expr) -> Self::Output {
        match &self {
            DBI(i) if dbi == *i => replacement.clone(),
            DBI(_) => self,

            UnaryExpr(op, unary) =>
                UnaryExpr(op.clone(), unary.clone().subst(dbi, replacement)),

            BinaryExpr(op, lhs, rhs) =>
                BinaryExpr(op.clone(), lhs.clone().subst(dbi, replacement),
                           rhs.clone().subst(dbi, replacement)),

            AtomExpr(AtomLambda(nested_argc, nested_dbi, nested_body)) => {
                AtomExpr(AtomLambda(*nested_argc, *nested_dbi,
                                    nested_body.clone().subst(nested_argc + dbi, replacement)))
            }

            ApplyExpr(f, arg) =>
                ApplyExpr(f.clone().subst(dbi, replacement),
                          arg.clone().subst(dbi, replacement)),

            _ => self,
        }
    }
}

impl Substitution {
    pub fn subst(dbi: i32, expr: Expr, replacement: &Expr) -> Expr {
        expr.subst(dbi, replacement)
    }
}
