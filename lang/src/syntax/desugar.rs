use crate::syntax::tree::{Decl, Expr, MatchCase, Program, ProgramItem};

pub struct Desugar;

impl Desugar {
    pub fn run(input: Program) -> Program {
        input.desugar()
    }
}

trait Desugarable {
    fn desugar(self) -> Self;
}

impl<T: Desugarable> Desugarable for Vec<T> {
    fn desugar(self) -> Self {
        self.into_iter().map(|d| d.desugar()).collect()
    }
}

impl<T: Desugarable> Desugarable for Box<T> {
    fn desugar(self) -> Self {
        Box::new((*self).desugar())
    }
}

impl Desugarable for ProgramItem {
    fn desugar(self) -> Self {
        match self {
            ProgramItem::ExprItem(expr) => ProgramItem::ExprItem(expr.desugar()),
            ProgramItem::DeclItem(decl) => ProgramItem::DeclItem(decl.desugar()),
        }
    }
}

impl Desugarable for Decl {
    fn desugar(self) -> Self {
        match self {
            Decl::LetDecl(name, val) => Decl::LetDecl(name, val.desugar()),
            Decl::EnumDecl(name, variants) => Decl::EnumDecl(name, variants),
            Decl::TraitDecl(name, fns) => Decl::TraitDecl(name, fns),
            Decl::ImplDecl(tr, ty, fns) => Decl::ImplDecl(tr, ty, fns.desugar()),
        }
    }
}

impl Desugarable for Expr {
    fn desugar(self) -> Self {
        match self {
            // we mainly desugar operators into trait fn calls
            Expr::UnaryExpr(op, operand) => desugar_unary(op, operand.desugar()),
            Expr::BinaryExpr(op, lhs, rhs) => desugar_binary(op, lhs.desugar(), rhs.desugar()),

            Expr::Unit => Expr::Unit,
            Expr::AtomExpr(atom) => Expr::AtomExpr(atom),
            Expr::DBI(dbi) => Expr::DBI(dbi),
            Expr::ApplyExpr(f, a) => Expr::ApplyExpr(f.desugar(), a.desugar()),
            Expr::MemberExpr(lhs, id) => Expr::MemberExpr(lhs.desugar(), id),
            Expr::MatchExpr(matchee, cases) => Expr::MatchExpr(matchee.desugar(), cases.desugar()),
        }
    }
}

impl Desugarable for MatchCase {
    fn desugar(self) -> Self {
        MatchCase(self.0, self.1.desugar())
    }
}

fn desugar_unary(op: String, operand: Box<Expr>) -> Expr {
    match op.as_str() {
        "!" => Expr::ApplyExpr(
            Box::new(Expr::MemberExpr(operand, "not".to_string())),
            Box::new(Expr::Unit),
        ),
        _ => unreachable!("Not supported unary operator"),
    }
}

fn desugar_binary(op: String, lhs: Box<Expr>, rhs: Box<Expr>) -> Expr {
    let name = match op.as_str() {
        "+" => "add",
        "-" => "sub",
        "*" => "mul",
        "/" => "div",
        "^" => "pow",
        "%" => "mod",
        "&&" => "and",
        "||" => "or",
        "==" => "cmp",
        "!=" => "cmp",
        "<=" => "cmp",
        "<" => "cmp",
        ">=" => "cmp",
        ">" => "cmp",
        _ => unreachable!("Not supported binary operator"),
    };

    Expr::ApplyExpr(Box::new(Expr::MemberExpr(lhs, name.to_string())), rhs)
}
