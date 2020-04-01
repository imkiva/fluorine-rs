use std::result::Result;
use std::collections::VecDeque;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use super::tree::nodes::*;

#[derive(Parser)]
#[grammar = "fs.pest"]
pub struct FsParser;

impl FsParser {
    pub fn ast(input: &str) -> Result<Program, CompileError> {
        let fs = FsParser::parse(Rule::unit, input);
        let pairs = fs.map_err(|e| CompileError(format!("{}", e)))?;
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

    let lhs = parse_binary_operand(exprs.pop_front().unwrap());

    ops.into_iter().fold(lhs, |lhs, op| {
        let rhs = parse_binary_operand(exprs.pop_front().unwrap());
        Expr::BinaryExpr(op.as_str().trim().to_owned(),
                         Box::new(lhs),
                         Box::new(rhs))
    })
}

fn parse_binary_operand(node: Pair<Rule>) -> Expr {
    match node.as_rule() {
        Rule::expr_unary => parse_expr_unary(node),
        Rule::expr_relational => parse_expr_binary(node),
        Rule::expr_binary_level1 => parse_expr_binary(node),
        Rule::expr_binary_level2 => parse_expr_binary(node),
        Rule::expr_binary_level3 => parse_expr_binary(node),
        _ => unreachable!("unsatisfied binary expr operands")
    }
}

fn parse_expr_unary(node: Pair<Rule>) -> Expr {
    let mut nodes: VecDeque<Pair<Rule>> = node.into_inner().into_iter().collect();
    let first = nodes.pop_front().unwrap();
    match first.as_rule() {
        Rule::unary_op => {
            assert_eq!(nodes.len(), 2);
            let operand = parse_expr_unary(nodes.pop_back().unwrap());
            Expr::UnaryExpr(first.as_str().trim().to_owned(),
                            Box::new(operand))
        }
        Rule::expr_atom => {
            let primary = parse_expr_atom(first);
            if let Some(apply) = nodes.pop_back() {
                apply.into_inner().into_iter().fold(primary, |lhs, arg| {
                    Expr::ApplyExpr(Box::new(lhs), Box::new(parse_expr(arg)))
                })
            } else {
                primary
            }
        }
        _ => unreachable!("expr unary inner should be expr_primary or unary_op"),
    }
}

fn parse_expr_atom(node: Pair<Rule>) -> Expr {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::expr => parse_expr(child),
        Rule::expr_lambda => parse_lambda(child),
        Rule::id => Expr::AtomExpr(Atom::AtomId(child.as_str().to_owned())),
        Rule::literal => parse_literal(child),
        _ => unreachable!("expr primary inner should be expr_quoted, expr_lambda, id or literal"),
    }
}

fn parse_lambda(node: Pair<Rule>) -> Expr {
    Expr::DBI(0)
}

fn parse_expr_list(node: Pair<Rule>) -> Vec<Expr> {
    Vec::new()
}

fn parse_literal(node: Pair<Rule>) -> Expr {
    let lit = node.into_inner().next().unwrap();

    Expr::AtomExpr(Atom::AtomLit(
        match lit.as_rule() {
            Rule::number_lit => Lit::LitNumber(lit.as_str().parse::<f64>().unwrap()),
            Rule::string_lit => Lit::LitString(lit.as_str().to_owned()),
            _ => unreachable!("unsupported literal type: {:?}", lit.as_rule()),
        }
    ))
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
