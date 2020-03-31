use std::result::Result;
use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};

use super::tree::nodes::*;
use crate::tree::nodes::Expr::DBI;

#[derive(Parser)]
#[grammar = "fs.pest"]
pub struct FsParser;

impl FsParser {
    pub fn to_tree(input: &str) -> Result<Program, CompileError> {
        let fs: Result<Pairs<Rule>, _> = FsParser::parse(Rule::unit, input);
        let pairs: Pairs<Rule> = fs.map_err(|e| CompileError(format!("{}", e)))?;
        Ok(parse_all(pairs))
    }
}

fn parse_all(pairs: Pairs<Rule>) -> Program {
    pairs
        .into_iter()
        .flat_map(|item| parse_item(item.into_inner()))
        .collect()
}

fn parse_item(pairs: Pairs<Rule>) -> Program {
    pairs.map(|p| p.as_rule())
        .map(|rule| {
            match rule {
                Rule::expr => ProgramItem::Expr(DBI(0)),
                Rule::decl => ProgramItem::Decl(Decl::Let("a".to_owned(), DBI(0))),
                Rule::EOI => ProgramItem::EOF,
                _ => unreachable!("rule should be expr or decl"),
            }
        })
        .collect()
}

#[derive(Debug)]
pub struct CompileError(String);
