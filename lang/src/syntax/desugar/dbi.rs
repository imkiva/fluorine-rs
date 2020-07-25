use crate::syntax::tree::{
    Atom::{AtomId, AtomRawLambda},
    Decl,
    Decl::{EnumDecl, ImplDecl, LetDecl, TraitDecl},
    Expr,
    Expr::{ApplyExpr, AtomExpr, AwaitExpr, BinaryExpr, MatchExpr, MemberExpr, UnaryExpr, DBI},
    MatchCase, Param, ParseRawLambda, Program,
    ProgramItem::{DeclItem, ExprItem},
};
use std::collections::VecDeque;

pub fn convert_dbi(input: Program) -> Program {
    input
        .into_iter()
        .map(|item| match item {
            ExprItem(expr) => ExprItem(dbi_lambda(&mut VecDeque::new(), expr)),

            DeclItem(decl) => DeclItem(convert_dbi_decl(decl)),
        })
        .collect()
}

fn convert_dbi_decl(decl: Decl) -> Decl {
    match decl {
        LetDecl(name, expr) => LetDecl(name, dbi_lambda(&mut VecDeque::new(), expr)),

        ImplDecl(generic, tr, ty, fns) => ImplDecl(
            generic,
            tr,
            ty,
            fns.into_iter().map(convert_dbi_decl).collect(),
        ),

        EnumDecl(generic, name, variants) => EnumDecl(generic, name, variants),

        TraitDecl(name, fns) => TraitDecl(name, fns),
    }
}

fn dbi_lambda(param_stack: &mut VecDeque<&Vec<Param>>, expr: Expr) -> Expr {
    match expr {
        // if this is a unsolved lambda
        AtomExpr(AtomRawLambda(raw)) => dbi_lambda_body(param_stack, raw),

        ApplyExpr(f, a) => ApplyExpr(
            Box::new(dbi_lambda(param_stack, *f)),
            Box::new(dbi_lambda(&mut VecDeque::new(), *a)),
        ),

        // not a lambda, just return what we have now
        _ => expr,
    }
}

fn dbi_lambda_body(param_stack: &mut VecDeque<&Vec<Param>>, raw: ParseRawLambda) -> Expr {
    // I know what I am doing!
    unsafe {
        // This is totally SAFE!!!!
        let ptr: *const Vec<Param> = &raw.params;
        param_stack.push_front(&*ptr);
    }

    // recursively convert variable name to dbi
    let body = raw
        .body
        .into_iter()
        .map(|raw| dbi_expr(param_stack, raw))
        .collect();

    // pop reference of raw.params
    let _ = param_stack.pop_front();

    AtomExpr(AtomRawLambda(ParseRawLambda {
        is_async: raw.is_async,
        params: raw.params,
        body,
    }))
}

fn dbi_expr(param_stack: &mut VecDeque<&Vec<Param>>, expr: Expr) -> Expr {
    match expr {
        // resolve variable name to dbi
        AtomExpr(AtomId(id)) => {
            if let Some(index) = resolve_param(param_stack, id.as_str()) {
                DBI(index)
            } else {
                AtomExpr(AtomId(id))
            }
        }

        UnaryExpr(op, unary) => {
            UnaryExpr(op.clone(), Box::new(dbi_expr(param_stack, *unary.clone())))
        }

        BinaryExpr(op, lhs, rhs) => BinaryExpr(
            op.clone(),
            Box::new(dbi_expr(param_stack, *lhs.clone())),
            Box::new(dbi_expr(param_stack, *rhs.clone())),
        ),

        ApplyExpr(f, a) => ApplyExpr(
            Box::new(dbi_expr(param_stack, *f.clone())),
            Box::new(dbi_expr(param_stack, *a.clone())),
        ),

        MatchExpr(matchee, cases) => MatchExpr(
            Box::new(dbi_expr(param_stack, *matchee.clone())),
            cases
                .into_iter()
                .map(|MatchCase(pat, expr)| {
                    MatchCase(pat.clone(), dbi_expr(param_stack, expr.clone()))
                })
                .collect(),
        ),

        MemberExpr(lhs, id) => MemberExpr(Box::new(dbi_expr(param_stack, *lhs)), id),

        AwaitExpr(expr) => AwaitExpr(Box::new(dbi_expr(param_stack, *expr))),

        // try match nested lambda
        _ => dbi_lambda(param_stack, expr),
    }
}

fn resolve_param(param_stack: &VecDeque<&Vec<Param>>, name: &str) -> Option<usize> {
    let mut base_dbi = 0;

    for &scope in param_stack {
        if let Some(index) = scope.into_iter().position(|r| r.id.as_str() == name) {
            return Some(base_dbi + index);
        }
        base_dbi += scope.len();
    }
    None
}
