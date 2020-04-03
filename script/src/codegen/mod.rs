use crate::tree::{Program, Decl, Expr, Atom, ProgramItem, Lit};

pub trait CodeGenerator {
    type Output;

    fn codegen(&self, input: Program) -> Self::Output;
}

pub trait PartialCodeGenerator {
    type Output;

    fn partial_codegen_decl(&self, _: Decl) -> Self::Output;

    fn partial_codegen_expr(&self, _: Expr) -> Self::Output;

    fn partial_codegen_atom(&self, _: Atom) -> Self::Output;

    fn partial_codegen_lit(&self, _: Lit) -> Self::Output;
}

impl<T> CodeGenerator for T where
    T: PartialCodeGenerator<Output=String> {
    type Output = String;

    fn codegen(&self, input: Program) -> String {
        input.into_iter()
            .map(|item| match item {
                ProgramItem::ExprItem(expr) => self.partial_codegen_expr(expr),
                ProgramItem::DeclItem(decl) => self.partial_codegen_decl(decl),
            })
            .fold(String::new(), |acc, s| acc + "\n" + s.as_str())
    }
}

pub mod fs;
