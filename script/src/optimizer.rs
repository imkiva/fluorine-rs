use super::tree::*;
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
                _ => item,
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
            fold_apply(0, optimize(*f), optimize(*a)),

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

fn fold_apply(start: i32, f: Expr, a: Expr) -> Expr {
    match f {
        AtomExpr(AtomLambda(ref argc, ref dbi, ref body)) =>
            if *dbi < *argc {
                // the lambda still accepts argument
                let mut new_body: Vec<Expr> = body.into_iter()
                    .map(|expr| subst(*argc, start + *dbi, expr.clone(), a.clone()))
                    .collect();

                if *dbi == *argc - 1 && new_body.len() == 1 {
                    // 1. full applied after subst
                    // 2. the lambda has only one expr in the body
                    // so we can inline it!
                    optimize(new_body.pop().unwrap())
                } else {
                    AtomExpr(AtomLambda(
                        *argc, dbi + 1,
                        new_body,
                    ))
                }
            } else {
                // full applied, do nothing
                ApplyExpr(Box::new(f), Box::new(a))
            },

        _ => ApplyExpr(Box::new(f), Box::new(a)),
    }
}

fn subst(argc: i32, dbi: i32, expr: Expr, a: Expr) -> Expr {
    match &expr {
        Expr::DBI(i) =>
            if *i == dbi {
                a
            } else {
                expr
            },

        UnaryExpr(op, unary) =>
            UnaryExpr(op.clone(), Box::new(subst(argc, dbi, *unary.clone(), a))),

        BinaryExpr(op, lhs, rhs) =>
            BinaryExpr(op.clone(), Box::new(subst(argc, dbi, *lhs.clone(), a.clone())),
                       Box::new(subst(argc, dbi, *rhs.clone(), a))),

        // ApplyExpr(f, nested) =>
        //     fold_apply(argc, subst(argc, dbi, *f.clone(), a.clone()),
        //                subst(argc, dbi, *nested.clone(), a)),

        _ => expr,
    }
}

fn optimize_decl(decl: Decl) -> Decl {
    match decl {
        Decl::LetDecl(name, expr) => Decl::LetDecl(name, optimize(expr)),
    }
}
