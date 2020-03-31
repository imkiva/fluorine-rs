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
        .map(|p| p.as_rule())
        .map(|rule| {
            match rule {
                Rule::expr => ProgramItem::ExprItem(parse_expr(rule)),
                Rule::decl => ProgramItem::DeclItem(parse_decl(rule)),
                Rule::EOI => ProgramItem::EOFItem,
                _ => unreachable!("rule should be expr or decl"),
            }
        })
        .collect()
}

fn parse_expr(rule: Rule) -> Expr {
    Expr::DBI(0)
}

fn parse_decl(rule: Rule) -> Decl {
    Decl::LetDecl("Name".to_owned(), Expr::DBI(0))
}

#[derive(Debug)]
pub struct CompileError(String);
