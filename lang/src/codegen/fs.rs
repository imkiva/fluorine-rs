use crate::{
    codegen::PartialCodeGenerator,
    runtime::subst::Subst,
    syntax::tree::{
        Atom,
        Atom::{AtomId, AtomLambda, AtomLit, AtomRawLambda},
        Constraint, Decl,
        Decl::{EnumDecl, ImplDecl, LetDecl, TraitDecl},
        EnumVariant, Expr,
        Expr::{
            ApplyExpr, AtomExpr, AwaitExpr, BinaryExpr, MatchExpr, MemberExpr, UnaryExpr, Unit, DBI,
        },
        GenericParam, Lit,
        Lit::{LitBool, LitNumber, LitString},
        MatchCase, Param, ParseType, Pattern, TraitFn,
    },
};
use std::collections::VecDeque;

pub struct FsCodeGenerator;

impl FsCodeGenerator {
    pub fn new() -> FsCodeGenerator {
        FsCodeGenerator {}
    }
}

impl PartialCodeGenerator for FsCodeGenerator {
    type Output = String;

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
            .fold(String::new(), |acc, t| match acc.len() {
                0 => t,
                _ => acc + "\n" + t.as_str(),
            })
    }
}

impl TargetFs for MatchCase {
    fn codegen_to_fs(self: Self) -> String {
        format!("{} => {}, ", self.0.codegen_to_fs(), self.1.codegen_to_fs())
    }
}

impl TargetFs for Pattern {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            Pattern::PatLit(lit) => lit.codegen_to_fs(),
            Pattern::PatWildcard => format!("_"),
            Pattern::PatVariant(variant) => {
                format!("{}({})", variant.name, variant.fields.join(", "))
            }
        }
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
            AtomLambda(param, dbi, body) => codegen_lambda(param, dbi, body),
            AtomRawLambda(raw) => codegen_raw_lambda(raw.is_async, raw.params, raw.body),
        }
    }
}

impl TargetFs for Expr {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            Unit => "()".to_string(),
            AtomExpr(atom) => atom.codegen_to_fs(),
            UnaryExpr(op, operand) => format!("{}{}", op, operand.codegen_to_fs()),
            BinaryExpr(op, lhs, rhs) => {
                format!("{} {} {}", lhs.codegen_to_fs(), op, rhs.codegen_to_fs())
            }
            ApplyExpr(f, a) => format!("{}({})", f.codegen_to_fs(), a.codegen_to_fs()),
            MatchExpr(matchee, cases) => format!(
                "match {} {{\n{}\n}}",
                matchee.codegen_to_fs(),
                cases.codegen_to_fs()
            ),
            MemberExpr(lhs, id) => format!("{}.{}", lhs.codegen_to_fs(), id.as_str()),
            AwaitExpr(expr) => format!("{}.await", expr.codegen_to_fs()),
            DBI(_) => unreachable!("dangling DBI outside lambda"),
        }
    }
}

impl TargetFs for Decl {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            LetDecl(name, expr) => format!("let {} = {}\n", name, expr.codegen_to_fs()),
            EnumDecl(generic, name, variants) => format!(
                "enum {}{} {{\n{}\n}}",
                name,
                generic.codegen_to_fs(),
                variants.codegen_to_fs()
            ),
            TraitDecl(name, fns) => format!("trait {} {{\n{}\n}}", name, fns.codegen_to_fs()),
            ImplDecl(generic, tr, ty, fns) => format!(
                "impl{} {} for {} {{\n{}\n}}",
                generic.codegen_to_fs(),
                tr,
                ty,
                fns.codegen_to_fs()
            ),
        }
    }
}

impl TargetFs for Vec<GenericParam> {
    fn codegen_to_fs(self: Self) -> String {
        if self.is_empty() {
            return String::new();
        }

        let code = self
            .into_iter()
            .map(|param| format!("{}{}", param.name, param.constraints.codegen_to_fs()))
            .collect::<Vec<_>>()
            .join(", ");

        format!("<{}>", code)
    }
}

impl TargetFs for Vec<Constraint> {
    fn codegen_to_fs(self: Self) -> String {
        if self.is_empty() {
            return String::new();
        }

        let code = self
            .into_iter()
            .map(|c| match c {
                Constraint::MustImpl(id) => id,
            })
            .collect::<Vec<_>>()
            .join(" + ");

        format!(": {}", code)
    }
}

impl TargetFs for EnumVariant {
    fn codegen_to_fs(self: Self) -> String {
        format!("{}({})", self.name, self.field_types.join(", "))
    }
}

impl TargetFs for TraitFn {
    fn codegen_to_fs(self: Self) -> String {
        format!(
            "fn {}({}) -> {};",
            self.name,
            self.param.codegen_to_fs(),
            self.ret.codegen_to_fs()
        )
    }
}

impl TargetFs for Param {
    fn codegen_to_fs(self: Self) -> String {
        format!(
            "{}{}",
            self.id,
            match self.ty {
                Some(ty) => format!(": {}", ty),
                _ => "".to_string(),
            }
        )
    }
}

impl TargetFs for ParseType {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            ParseType::SelfType => "Self".to_string(),
            ParseType::OtherType(ty) => ty,
        }
    }
}

fn codegen_lambda(param: Vec<Param>, dbi: usize, body: Vec<Expr>) -> String {
    let argc = param.len();
    let mut param = param.into_iter().collect::<VecDeque<_>>();
    for _ in 0..dbi {
        let _ = param.pop_front();
    }

    let body = (dbi..argc)
        .into_iter()
        .zip(param.iter())
        .fold(body, |body, (dbi, p)| {
            let replacement = AtomExpr(AtomId(p.id.clone()));
            body.into_iter()
                .map(|expr| expr.subst(dbi, &replacement))
                .collect()
        });

    codegen_raw_lambda(false, param.into_iter().collect(), body)
}

fn codegen_raw_lambda(is_async: bool, param: Vec<Param>, body: Vec<Expr>) -> String {
    format!(
        "{}{{ {} -> {} }}",
        if is_async { "async " } else { "" },
        param
            .into_iter()
            .map(|param| format!(
                "{}{}",
                param.id,
                match param.ty {
                    Some(ty) => format!(": {}", ty),
                    _ => format!(""),
                }
            ))
            .collect::<Vec<_>>()
            .join(", "),
        body.codegen_to_fs()
    )
}
