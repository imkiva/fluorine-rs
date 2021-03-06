use crate::syntax::tree::{
    Atom::{AtomLambda, AtomRawLambda},
    Expr,
    Expr::{
        ApplyExpr, AtomExpr, AwaitExpr, BinaryExpr, MatchExpr, MemberExpr, UnaryExpr, Unit, DBI,
    },
    MatchCase,
};

pub trait Subst {
    type Output;

    fn subst(self: Self, dbi: usize, replacement: &Expr) -> Self::Output;
}

impl<T: Subst<Output = T>> Subst for Box<T> {
    type Output = Box<T>;

    fn subst(self: Self, dbi: usize, replacement: &Expr) -> Self::Output {
        let t = *self;
        Box::new(t.subst(dbi, replacement))
    }
}

impl<T: Subst<Output = T>> Subst for Vec<T> {
    type Output = Vec<T>;

    fn subst(self: Self, dbi: usize, replacement: &Expr) -> Self::Output {
        self.into_iter()
            .map(|expr| expr.subst(dbi, replacement))
            .collect()
    }
}

impl Subst for MatchCase {
    type Output = MatchCase;

    fn subst(self: Self, dbi: usize, replacement: &Expr) -> Self::Output {
        MatchCase(self.0, self.1.subst(dbi, replacement))
    }
}

impl Subst for Expr {
    type Output = Expr;

    fn subst(self: Self, dbi: usize, replacement: &Expr) -> Self::Output {
        match self {
            UnaryExpr(_, _) => unreachable!("Desugar bug: unary expr"),
            BinaryExpr(_, _, _) => unreachable!("Desugar bug: binary expr"),
            AtomExpr(AtomRawLambda(_)) => unreachable!("Desugar bug: raw lambda"),

            // let eval.rs to handle await outside async.
            AwaitExpr(expr) => AwaitExpr(expr),

            Unit => Unit,
            DBI(i) if dbi == i => replacement.clone(),
            DBI(_) => self,

            AtomExpr(AtomLambda(nested_param, nested_dbi, nested_body)) => {
                let nested_argc = nested_param.len();
                AtomExpr(AtomLambda(
                    nested_param,
                    nested_dbi,
                    nested_body.subst(nested_argc + dbi, replacement),
                ))
            }

            AtomExpr(atom) => AtomExpr(atom),

            ApplyExpr(f, arg) => ApplyExpr(f.subst(dbi, replacement), arg.subst(dbi, replacement)),

            MatchExpr(matchee, cases) => MatchExpr(
                matchee.subst(dbi, replacement),
                cases.subst(dbi, replacement),
            ),

            MemberExpr(lhs, id) => MemberExpr(lhs.subst(dbi, replacement), id),
        }
    }
}
