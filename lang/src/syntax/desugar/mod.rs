use crate::syntax::{
    desugar::{async_await::desugar_async_lambda, dbi::convert_dbi},
    tree::{
        Atom::{AtomLambda, AtomRawLambda},
        Decl,
        Decl::{EnumDecl, ImplDecl, LetDecl, TraitDecl},
        Expr,
        Expr::{ApplyExpr, AtomExpr, AwaitExpr, BinaryExpr, MatchExpr, MemberExpr, UnaryExpr, DBI},
        MatchCase, ParseRawLambda, Program, ProgramItem,
        ProgramItem::{DeclItem, ExprItem},
    },
};

mod async_await;
mod dbi;

pub struct Desugar;

impl Desugar {
    pub fn run(input: Program) -> Program {
        let mut extra = vec![];
        let desugared = convert_dbi(input).desugar(&mut extra);
        extra.extend(desugared.into_iter());
        extra
    }
}

trait Desugarable {
    fn desugar(self, extra: &mut Program) -> Self;
}

impl<T: Desugarable> Desugarable for Vec<T> {
    fn desugar(self, extra: &mut Program) -> Self {
        self.into_iter().map(|d| d.desugar(extra)).collect()
    }
}

impl<T: Desugarable> Desugarable for Box<T> {
    fn desugar(self, extra: &mut Program) -> Self {
        Box::new((*self).desugar(extra))
    }
}

impl Desugarable for ProgramItem {
    fn desugar(self, extra: &mut Program) -> Self {
        match self {
            ExprItem(expr) => ExprItem(expr.desugar(extra)),
            DeclItem(decl) => DeclItem(decl.desugar(extra)),
        }
    }
}

impl Desugarable for Decl {
    fn desugar(self, extra: &mut Program) -> Self {
        match self {
            LetDecl(name, val) => LetDecl(name, val.desugar(extra)),
            EnumDecl(generic, name, variants) => EnumDecl(generic, name, variants),
            TraitDecl(name, fns) => TraitDecl(name, fns),
            ImplDecl(generic, tr, ty, fns) => ImplDecl(generic, tr, ty, fns.desugar(extra)),
        }
    }
}

impl Desugarable for Expr {
    fn desugar(self, extra: &mut Program) -> Self {
        match self {
            Expr::Unit => Expr::Unit,
            // we mainly desugar operators into trait fn calls
            UnaryExpr(op, operand) => desugar_unary(op, operand.desugar(extra)),
            BinaryExpr(op, lhs, rhs) => desugar_binary(op, lhs.desugar(extra), rhs.desugar(extra)),
            ApplyExpr(f, a) => ApplyExpr(f.desugar(extra), a.desugar(extra)),
            MemberExpr(lhs, id) => MemberExpr(lhs.desugar(extra), id),
            MatchExpr(matchee, cases) => MatchExpr(matchee.desugar(extra), cases.desugar(extra)),
            AtomExpr(AtomRawLambda(raw)) => desugar_raw_lambda(raw, extra).desugar(extra),
            AtomExpr(AtomLambda(params, dbi, body)) => {
                AtomExpr(AtomLambda(params, dbi, body.desugar(extra)))
            }
            AtomExpr(atom) => Expr::AtomExpr(atom),
            DBI(dbi) => Expr::DBI(dbi),

            // let eval.rs to handle await outside async.
            AwaitExpr(body) => AwaitExpr(body),
        }
    }
}

impl Desugarable for MatchCase {
    fn desugar(self, extra: &mut Program) -> Self {
        MatchCase(self.0, self.1.desugar(extra))
    }
}

fn desugar_raw_lambda(raw: ParseRawLambda, extra: &mut Program) -> Expr {
    match raw.is_async {
        true => desugar_async_lambda(raw.params, raw.body, extra),
        false => Expr::AtomExpr(AtomLambda(raw.params, 0, raw.body)),
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
