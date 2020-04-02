use crate::codegen::{PartialCodeGenerator, StringOutputGenerator};
use crate::tree::{Expr, Decl, Atom, Lit};
use crate::tree::Decl::LetDecl;
use crate::tree::Lit::{LitNumber, LitString, LitBool};
use crate::tree::Atom::{AtomLit, AtomId, AtomLambda, AtomRawLambda};
use crate::tree::Expr::{AtomExpr, UnaryExpr, BinaryExpr, ApplyExpr, DBI};
use std::ops::Range;

pub struct FsCodeGenerator;

impl StringOutputGenerator for FsCodeGenerator {}

impl PartialCodeGenerator<String> for FsCodeGenerator {
    fn partial_codegen_decl(&self, decl: Decl) -> String {
        decl.codegen_to_fs()
    }

    fn partial_codegen_expr(&self, expr: Expr) -> String {
        expr.codegen_to_fs()
    }

    fn partial_codegen_atom(&self, atom: Atom) -> String {
        atom.codegen_to_fs()
    }

    fn partial_codegen_lit(&self, lit: Lit) -> String {
        lit.codegen_to_fs()
    }
}

trait TargetFs {
    fn codegen_to_fs(self: Self) -> String;
}

impl<T: TargetFs> TargetFs for Box<T> {
    fn codegen_to_fs(self: Self) -> String {
        (*self).codegen_to_fs()
    }
}

impl<T: TargetFs> TargetFs for Vec<T> {
    fn codegen_to_fs(self: Self) -> String {
        self.into_iter()
            .map(|t| t.codegen_to_fs())
            .fold(String::new(), |acc, t| acc + "\n" + t.as_str())
    }
}

impl TargetFs for Lit {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            LitNumber(v) => v.to_string(),
            LitString(v) => v,
            LitBool(v) => v.to_string(),
        }
    }
}

impl TargetFs for Atom {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            AtomLit(lit) => lit.codegen_to_fs(),
            AtomId(id) => id,
            AtomLambda(argc, dbi, body) =>
                codegen_lambda(argc, dbi, body),
            AtomRawLambda(param, body) =>
                codegen_raw_lambda(param, body),
        }
    }
}

impl TargetFs for Expr {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            AtomExpr(atom) => atom.codegen_to_fs(),
            UnaryExpr(op, operand) =>
                format!("{}{}", op, operand.codegen_to_fs()),
            BinaryExpr(op, lhs, rhs) =>
                format!("{} {} {}", lhs.codegen_to_fs(), op, rhs.codegen_to_fs()),
            ApplyExpr(f, a) =>
                format!("{}({})", f.codegen_to_fs(), a.codegen_to_fs()),
            DBI(i) => unreachable!("dangling DBI outside lambda"),
            _ => unreachable!("internal error expr"),
        }
    }
}

impl TargetFs for Decl {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            LetDecl(name, expr) =>
                format!("let {} = {}\n", name, expr.codegen_to_fs()),
        }
    }
}

fn codegen_lambda(argc: i32, dbi: i32, body: Vec<Expr>) -> String {
    let (param, body) = (dbi..argc).into_iter()
        .map(|i| (i, format!("a{}", i)))
        .fold((Vec::new(), body),
              |(mut params, body), (dbi, name)| {
                  params.push(name.clone());
                  let replacement = AtomExpr(AtomId(name));
                  (params, body.into_iter()
                      .map(|expr| subst(dbi, expr, &replacement))
                      .collect())
              });

    codegen_raw_lambda(param, body)
}

fn codegen_raw_lambda(param: Vec<String>, body: Vec<Expr>) -> String {
    format!("{{ {} -> {} }}",
            param.join(", "),
            body.codegen_to_fs())
}

fn subst(dbi: i32, expr: Expr, replacement: &Expr) -> Expr {
    match &expr {
        DBI(i) if dbi == *i => replacement.clone(),
        DBI(_) => expr,

        UnaryExpr(op, unary) =>
            UnaryExpr(op.clone(), Box::new(subst(dbi, *unary.clone(), replacement))),

        BinaryExpr(op, lhs, rhs) =>
            BinaryExpr(op.clone(), Box::new(subst(dbi, *lhs.clone(), replacement)),
                       Box::new(subst(dbi, *rhs.clone(), replacement))),

        AtomExpr(AtomLambda(ret_argc, ret_dbi, ret_body)) => {
            let new_body: Vec<Expr> = ret_body.into_iter()
                .map(|ret_expr| subst(ret_argc + dbi, ret_expr.clone(), replacement))
                .collect();
            AtomExpr(AtomLambda(*ret_argc, *ret_dbi, new_body))
        }

        ApplyExpr(f, arg) =>
            ApplyExpr(Box::new(subst(dbi, *f.clone(), replacement)),
                      Box::new(subst(dbi, *arg.clone(), replacement))),

        _ => expr,
    }
}
