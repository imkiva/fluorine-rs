use crate::tree::*;
use crate::tree::ProgramItem::{DeclItem, ExprItem};
use crate::tree::Expr::{UnaryExpr, BinaryExpr, ApplyExpr, AtomExpr};
use crate::tree::Atom::{AtomLit, AtomLambda};
use crate::tree::Lit::{LitBool, LitNumber};

pub struct Optimizer;

impl Optimizer {
    pub fn run(input: Program) -> Program {
        input.into_iter()
            .map(|item| match item {
                ExprItem(expr) => ExprItem(optimize(expr)),
                DeclItem(decl) => DeclItem(optimize_decl(decl)),
            })
            .collect()
    }
}

fn optimize(expr: Expr) -> Expr {
    match expr {
        UnaryExpr(op, operand) =>
            fold_unary(op, optimize(*operand)),

        BinaryExpr(op, lhs, rhs) =>
            fold_binary(op, optimize(*lhs), optimize(*rhs)),

        ApplyExpr(f, a) =>
            fold_apply(optimize(*f), optimize(*a)),

        _ => expr
    }
}

fn fold_unary(op: String, operand: Expr) -> Expr {
    match (op.as_str(), &operand) {
        ("!", AtomExpr(AtomLit(LitBool(b)))) =>
            AtomExpr(AtomLit(LitBool(!*b))),
        _ => UnaryExpr(op, Box::new(operand)),
    }
}

fn fold_binary(op: String, lhs: Expr, rhs: Expr) -> Expr {
    match (op.as_str(), &lhs, &rhs) {
        ("&&", AtomExpr(AtomLit(LitBool(false))), _) => AtomExpr(AtomLit(LitBool(false))),
        ("&&", AtomExpr(AtomLit(LitBool(true))), _) => rhs,
        ("||", AtomExpr(AtomLit(LitBool(true))), _) => AtomExpr(AtomLit(LitBool(true))),
        ("||", AtomExpr(AtomLit(LitBool(false))), _) => rhs,

        ("&&", _, AtomExpr(AtomLit(LitBool(false)))) => AtomExpr(AtomLit(LitBool(false))),
        ("&&", _, AtomExpr(AtomLit(LitBool(true)))) => lhs,
        ("||", _, AtomExpr(AtomLit(LitBool(true)))) => AtomExpr(AtomLit(LitBool(true))),
        ("||", _, AtomExpr(AtomLit(LitBool(false)))) => lhs,

        ("+", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(a + b))),
        ("-", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(a - b))),
        ("*", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(a * b))),
        ("/", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(a / b))),
        ("%", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(a % b))),
        ("^", AtomExpr(AtomLit(LitNumber(a))), AtomExpr(AtomLit(LitNumber(b)))) =>
            AtomExpr(AtomLit(LitNumber(f64::powf(*a, *b)))),

        ("==", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a == *b))),
        ("!=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a != *b))),
        (">=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a >= *b))),
        (">", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a > *b))),
        ("<=", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a <= *b))),
        ("<", AtomExpr(AtomLit(a)), AtomExpr(AtomLit(b))) =>
            AtomExpr(AtomLit(LitBool(*a < *b))),

        _ => BinaryExpr(op, Box::new(lhs), Box::new(rhs)),
    }
}

fn fold_apply(f: Expr, a: Expr) -> Expr {
    match f {
        AtomExpr(AtomLambda(ref argc, ref dbi, ref body))
        if *dbi < *argc => {
            // the lambda still accepts argument
            let mut new_body: Vec<Expr> = body.into_iter()
                .map(|expr| subst(*dbi, expr.clone(), &a))
                .collect();

            if *dbi == *argc - 1 && new_body.len() == 1 {
                // 1. full applied after subst
                // 2. the lambda has only one expr in the body
                // so we can inline it!
                optimize(new_body.pop().unwrap())
            } else {
                AtomExpr(AtomLambda(
                    *argc,
                    *dbi + 1,
                    new_body,
                ))
            }
        }

        AtomExpr(AtomLambda(_, _, _)) =>
            ApplyExpr(Box::new(f), Box::new(a)),

        _ => ApplyExpr(Box::new(f), Box::new(a))
    }
}

/// This is a specialized version of Subst::subst
/// Consider reuse the standard version in the future.
fn subst(dbi: i32, expr: Expr, replacement: &Expr) -> Expr {
    match &expr {
        Expr::DBI(i) if dbi == *i => replacement.clone(),
        Expr::DBI(_) => expr,

        UnaryExpr(op, unary) =>
            UnaryExpr(op.clone(), Box::new(optimize(subst(dbi, *unary.clone(), replacement)))),

        BinaryExpr(op, lhs, rhs) =>
            BinaryExpr(op.clone(), Box::new(optimize(subst(dbi, *lhs.clone(), replacement))),
                       Box::new(optimize(subst(dbi, *rhs.clone(), replacement)))),

        AtomExpr(AtomLambda(ret_argc, ret_dbi, ret_body)) => {
            let new_body: Vec<Expr> = ret_body.into_iter()
                .map(|ret_expr| optimize(subst(ret_argc + dbi, ret_expr.clone(), replacement)))
                .collect();
            AtomExpr(AtomLambda(*ret_argc, *ret_dbi, new_body))
        }

        ApplyExpr(f, arg) =>
            fold_apply(optimize(subst(dbi, *f.clone(), replacement)),
                       optimize(subst(dbi, *arg.clone(), replacement))),

        _ => expr,
    }
}

fn optimize_decl(decl: Decl) -> Decl {
    match decl {
        Decl::LetDecl(name, expr) => Decl::LetDecl(name, optimize(expr)),
    }
}
