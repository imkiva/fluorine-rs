use crate::{
    codegen::PartialCodeGenerator,
    runtime::subst::Subst,
    syntax::tree::{
        Atom,
        Atom::{AtomId, AtomLambda, AtomLit, AtomRawLambda},
        Decl,
        Decl::{EnumDecl, LetDecl},
        EnumVariant, Expr,
        Expr::{ApplyExpr, AtomExpr, BinaryExpr, MatchExpr, UnaryExpr, Unit, DBI},
        Lit,
        Lit::{LitBool, LitNumber, LitString},
        MatchCase, Pattern,
    },
};

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
            AtomLambda(argc, dbi, body) => codegen_lambda(argc, dbi, body),
            AtomRawLambda(param, body) => codegen_raw_lambda(param, body),
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
            DBI(_) => unreachable!("dangling DBI outside lambda"),
        }
    }
}

impl TargetFs for Decl {
    fn codegen_to_fs(self: Self) -> String {
        match self {
            LetDecl(name, expr) => format!("let {} = {}\n", name, expr.codegen_to_fs()),
            EnumDecl(name, variants) => {
                format!("enum {} {{\n{}\n}}", name, variants.codegen_to_fs())
            }
        }
    }
}

impl TargetFs for EnumVariant {
    fn codegen_to_fs(self: Self) -> String {
        format!(
            "{}({})",
            self.name,
            (0..self.fields)
                .map(|_| "_".to_owned())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn codegen_lambda(argc: usize, dbi: usize, body: Vec<Expr>) -> String {
    let (param, body) = (dbi..argc)
        .into_iter()
        .map(|i| (i, format!("a{}", i)))
        .fold((Vec::new(), body), |(mut params, body), (dbi, name)| {
            params.push(name.clone());
            let replacement = AtomExpr(AtomId(name));
            (
                params,
                body.into_iter()
                    .map(|expr| expr.subst(dbi, &replacement))
                    .collect(),
            )
        });

    codegen_raw_lambda(param, body)
}

fn codegen_raw_lambda(param: Vec<String>, body: Vec<Expr>) -> String {
    format!("{{ {} -> {} }}", param.join(", "), body.codegen_to_fs())
}
