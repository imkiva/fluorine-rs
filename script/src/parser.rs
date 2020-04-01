use std::result::Result;
use pest::Parser;
use pest::iterators::{Pair, Pairs};

use super::tree::nodes::*;
use std::borrow::Borrow;
use std::collections::VecDeque;

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
    parse_expr_binary(node)
}

fn parse_expr_binary(node: Pair<Rule>) -> Expr {
    let mut exprs = VecDeque::new();
    let mut ops = Vec::new();

    for child in node.into_inner() {
        match child.as_rule() {
            Rule::expr_relational => exprs.push_front(child),
            Rule::expr_binary_level1 => exprs.push_front(child),
            Rule::expr_binary_level2 => exprs.push_front(child),
            Rule::expr_binary_level3 => exprs.push_front(child),
            Rule::expr_unary => exprs.push_front(child),

            Rule::logical_op => ops.push(child),
            Rule::relational_op => ops.push(child),
            Rule::level1_op => ops.push(child),
            Rule::level2_op => ops.push(child),
            Rule::level3_op => ops.push(child),

            _ => unreachable!("unsatisfied binary expr operands or operators"),
        }
    }

    let lhs = parse_bianry_operand(exprs.pop_front().unwrap());

    ops.into_iter().fold(lhs, |lhs, op| {
        let rhs = parse_bianry_operand(exprs.pop_front().unwrap());
        Expr::BinaryExpr(op.as_str().trim().to_owned(),
                         Box::new(lhs),
                         Box::new(rhs))
    })
}

fn parse_bianry_operand(node: Pair<Rule>) -> Expr {
    match node.as_rule() {
        Rule::expr_unary => parse_expr_unary(node),
        Rule::expr_relational => parse_expr_binary(node),
        Rule::expr_binary_level1 => parse_expr_binary(node),
        Rule::expr_binary_level2 => parse_expr_binary(node),
        Rule::expr_binary_level3 => parse_expr_binary(node),
        Rule::expr_unary => parse_expr_binary(node),
        _ => unreachable!("unsatisfied binary expr operands")
    }
}

fn parse_expr_unary(node: Pair<Rule>) -> Expr {
    Expr::DBI(0)
}

fn parse_decl(node: Pair<Rule>) -> Decl {
    let mut id = "";
    let mut expr = Expr::_InternalError;
    for child in node.into_inner() {
        match child.as_rule() {
            Rule::id => id = child.as_str(),
            Rule::expr => expr = parse_expr(child),
            _ => unreachable!("decl inner should be id or expr")
        }
    }
    Decl::LetDecl(id.to_owned(), expr)
}

#[derive(Debug)]
pub struct CompileError(String);
