use super::tree::*;
use crate::tree::ProgramItem::{DeclItem, ExprItem};
use crate::tree::Expr::{UnaryExpr, BinaryExpr, ApplyExpr, AtomExpr};
use crate::tree::Atom::AtomLit;
use crate::tree::Lit::LitBool;

pub struct Optimizer;

trait InnerType {
    type Type;
}

trait Functor<T: InnerType> {
    fn fmap<F: Fn(<T as InnerType>::Type) -> <T as InnerType>::Type>(self, f: F) -> T;
}

impl<T> InnerType for Box<T> {
    type Type = T;
}

impl<T> Functor<Box<T>> for Box<T> {
    fn fmap<F: Fn(T) -> T>(self, f: F) -> Box<T> {
        Box::new(f(*self))
    }
}

impl Optimizer {
    pub fn run(input: Program) -> Program {
        input.into_iter()
            .map(|item| match item {
                ExprItem(expr) => ExprItem(optimize(expr)),
                DeclItem(decl) => DeclItem(optimize_decl(decl)),
                _ => item,
            })
            .collect()
    }
}

fn optimize(expr: Expr) -> Expr {
    match expr {
        UnaryExpr(op, expr) =>
            UnaryExpr(op, expr.fmap(optimize)),

        BinaryExpr(op, lhs, rhs) =>
            fold_binary(op, optimize(*lhs), optimize(*rhs)),

        ApplyExpr(f, a) =>
            fold_apply(optimize(*f), optimize(*a)),

        _ => expr
    }
}

fn fold_binary(op: String, lhs: Expr, rhs: Expr) -> Expr {
    match (op.as_str(), &lhs, &rhs) {
        ("&&", AtomExpr(AtomLit(LitBool(false))), _) => AtomExpr(AtomLit(LitBool(false))),
        ("&&", AtomExpr(AtomLit(LitBool(true))), _) => rhs,
        ("||", AtomExpr(AtomLit(LitBool(true))), _) => AtomExpr(AtomLit(LitBool(true))),
        ("||", AtomExpr(AtomLit(LitBool(false))), _) => rhs,

        _ => BinaryExpr(op, Box::new(lhs), Box::new(rhs)),
    }
}

fn fold_apply(f: Expr, a: Expr) -> Expr {
    ApplyExpr(Box::new(f), Box::new(a))
}

fn optimize_decl(decl: Decl) -> Decl {
    match decl {
        Decl::LetDecl(name, expr) => Decl::LetDecl(name, optimize(expr)),
    }
}
