use crate::tree::{Program, Decl, Expr, Atom, ProgramItem, Lit};

pub trait StringOutputGenerator {}

pub trait CodeGenerator<Output> {
    fn codegen(&self, input: Program) -> Output;
}

pub trait PartialCodeGenerator<Output> {
    fn partial_codegen_decl(&self, _: Decl) -> Output;

    fn partial_codegen_expr(&self, _: Expr) -> Output;

    fn partial_codegen_atom(&self, _: Atom) -> Output;

    fn partial_codegen_lit(&self, _: Lit) -> Output;
}

impl<T> CodeGenerator<String> for T where
    T: StringOutputGenerator + PartialCodeGenerator<String> {
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
