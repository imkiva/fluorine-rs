use std::result::Result;
use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};

use super::tree::nodes::*;

#[derive(Parser)]
#[grammar = "fs.pest"]
pub struct FsParser;

impl FsParser {
    pub fn ast(input: &str) -> Result<Program, CompileError> {
        let fs: Result<Pairs<Rule>, _> = FsParser::parse(Rule::unit, input);
        let pairs: Pairs<Rule> = fs.map_err(|e| CompileError(format!("{}", e)))?;
        Ok(parse_unit(pairs))
    }
}

fn parse_unit(pairs: Pairs<Rule>) -> Program {
    pairs.into_iter()
        .flat_map(|item| item.into_inner())
        .map(|node| {
            match node.as_rule() {
                Rule::expr => ProgramItem::ExprItem(parse_expr(node)),
                Rule::decl => ProgramItem::DeclItem(parse_decl(node)),
                Rule::EOI => ProgramItem::EOFItem,
                _ => unreachable!("rule should be expr or decl"),
            }
        })
        .collect()
}

fn parse_expr(node: Pair<Rule>) -> Expr {
    Expr::DBI(0)
}

fn parse_decl(node: Pair<Rule>) -> Decl {
    let mut id = "".to_string();
    let mut expr = Expr::_InternalError;
    for child in node.into_inner() {
        match child.as_rule() {
            Rule::id => id = child.as_str().to_owned(),
            Rule::expr => expr = parse_expr(child),
            _ => unreachable!("decl inner should be id or expr")
        }
    }
    Decl::LetDecl(id, expr)
}

#[derive(Debug)]
pub struct CompileError(String);
