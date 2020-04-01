use std::result::Result;
use std::collections::VecDeque;
use pest::Parser;
use pest::iterators::{Pair, Pairs};
use crate::tree::*;

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
            Rule::expr_relational => exprs.push_back(child),
            Rule::expr_binary_level1 => exprs.push_back(child),
            Rule::expr_binary_level2 => exprs.push_back(child),
            Rule::expr_binary_level3 => exprs.push_back(child),
            Rule::expr_unary => exprs.push_back(child),

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
            nodes.into_iter()
                .flat_map(|apply| apply.into_inner())
                .fold(primary, |lhs, arg| {
                    Expr::ApplyExpr(Box::new(lhs), Box::new(parse_expr(arg)))
                })
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
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::normal_lambda => dbi_lambda(&mut VecDeque::new(),
                                          parse_normal_lambda(child)),
        Rule::quick_lambda => parse_quick_lambda(child),
        _ => unreachable!("lambda inner should be normal_lambda or quick_lambda")
    }
}

fn parse_normal_lambda(node: Pair<Rule>) -> Expr {
    let mut nodes: VecDeque<Pair<Rule>> = node.into_inner().into_iter().collect();
    let params: Vec<Name> = nodes.pop_front().unwrap().into_inner().into_iter()
        .map(|id| id.as_str().to_owned())
        .collect();
    let body = parse_expr_list(nodes.pop_back().unwrap());
    Expr::AtomExpr(Atom::AtomRawLambda(params, body))
}

fn parse_quick_lambda(node: Pair<Rule>) -> Expr {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::logical_op => (),
        Rule::relational_op => (),
        Rule::level1_op => (),
        Rule::level2_op => (),
        Rule::level3_op => (),
        _ => unreachable!("unsupported quick lambda operator: {}", child.as_str())
    }
    let body = Expr::BinaryExpr(child.as_str().trim().to_owned(),
                                Box::new(Expr::DBI(0)),
                                Box::new(Expr::DBI(1)));
    let lam = Atom::AtomLambda(2, vec![body]);
    Expr::AtomExpr(lam)
}

fn parse_expr_list(node: Pair<Rule>) -> Vec<Expr> {
    node.into_inner().into_iter()
        .map(|expr| parse_expr(expr))
        .collect()
}

fn parse_literal(node: Pair<Rule>) -> Expr {
    let lit = node.into_inner().next().unwrap();

    Expr::AtomExpr(Atom::AtomLit(
        match lit.as_rule() {
            Rule::number_lit => Lit::LitNumber(lit.as_str().parse::<f64>().unwrap()),
            Rule::string_lit => Lit::LitString(lit.as_str().to_owned()),
            Rule::bool_lit => Lit::LitBool(lit.as_str().parse::<bool>().unwrap()),
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

fn dbi_lambda(param_stack: &mut VecDeque<&Vec<Name>>, expr: Expr) -> Expr {
    match expr {
        // if this is a unsolved lambda
        Expr::AtomExpr(Atom::AtomRawLambda(names, body)) =>
            dbi_fuck_rustc(param_stack, names, body),

        // not a lambda, just return what we have now
        _ => expr,
    }
}

fn dbi_fuck_rustc(param_stack: &mut VecDeque<&Vec<Name>>, names: Vec<Name>, body: Vec<Expr>) -> Expr {
    // I know what I am doing!
    unsafe {
        // This is totally SAFE!!!!
        let ptr: *const Vec<Name> = &names;
        param_stack.push_front(&*ptr);
    }

    // recursively convert variable name to dbi
    let r = Expr::AtomExpr(Atom::AtomLambda(
        names.len() as i32,
        body.iter()
            .map(|raw| dbi_expr(param_stack, raw.clone()))
            .collect(),
    ));

    let _ = param_stack.pop_front();
    r
}

fn dbi_expr(param_stack: &mut VecDeque<&Vec<Name>>, expr: Expr) -> Expr {
    match &expr {
        // resolve variable name to dbi
        Expr::AtomExpr(Atom::AtomId(id)) => {
            if let Some(index) = resolve_param(param_stack, id.as_str()) {
                Expr::DBI(index)
            } else {
                expr
            }
        }

        Expr::UnaryExpr(op, unary) =>
            Expr::UnaryExpr(op.clone(), Box::new(dbi_expr(param_stack, *unary.clone()))),

        Expr::BinaryExpr(op, lhs, rhs) =>
            Expr::BinaryExpr(op.clone(), Box::new(dbi_expr(param_stack, *lhs.clone())),
                             Box::new(dbi_expr(param_stack, *rhs.clone()))),

        Expr::ApplyExpr(f, a) =>
            Expr::ApplyExpr(Box::new(dbi_expr(param_stack, *f.clone())),
                            Box::new(dbi_expr(param_stack, *a.clone()))),

        // try match nested lambda
        _ => dbi_lambda(param_stack, expr),
    }
}

fn resolve_param(param_stack: &VecDeque<&Vec<Name>>, name: &str) -> Option<i32> {
    let mut base_dbi = 0;

    for &scope in param_stack {
        if let Some(index) = scope.into_iter().position(|r| r == name) {
            return Some(base_dbi + index as i32);
        }
        base_dbi += scope.len() as i32;
    }
    None
}

#[derive(Debug)]
pub struct CompileError(String);
