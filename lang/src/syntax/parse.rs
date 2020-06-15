use crate::syntax::tree::{
    Atom::*,
    Decl::*,
    Expr::*,
    Lit::*,
    Pattern::{PatLit, PatVariant, PatWildcard},
    ProgramItem::*,
    *,
};
use pest::{
    error::{Error, ErrorVariant},
    iterators::{Pair, Pairs},
    Parser,
};
use std::{collections::VecDeque, result::Result};

#[derive(Parser)]
#[grammar = "syntax/grammar.pest"]
pub struct FsParser;

pub type ParseErrorVariant = ErrorVariant<Rule>;
pub type ParseError = Error<Rule>;

#[derive(Debug)]
pub struct CompileError(pub ParseError);

impl FsParser {
    pub fn ast(input: &str) -> Result<Program, CompileError> {
        let fs = FsParser::parse(Rule::unit, input);
        let pairs = fs.map_err(|e| CompileError(e))?;
        Ok(convert_dbi(parse_unit(pairs)))
    }
}

fn convert_dbi(input: Program) -> Program {
    input
        .into_iter()
        .map(|item| match item {
            ExprItem(expr) => ExprItem(dbi_lambda(&mut VecDeque::new(), expr)),

            DeclItem(LetDecl(name, expr)) => {
                DeclItem(LetDecl(name, dbi_lambda(&mut VecDeque::new(), expr)))
            }

            DeclItem(EnumDecl(name, variants)) => DeclItem(EnumDecl(name, variants)),
        })
        .collect()
}

fn parse_unit(pairs: Pairs<Rule>) -> Program {
    pairs
        .into_iter()
        .flat_map(|item| item.into_inner())
        .filter_map(|node| match node.as_rule() {
            Rule::expr => Some(ExprItem(parse_expr(node))),
            Rule::decl => Some(DeclItem(parse_decl(node))),
            Rule::EOI => None,
            _ => unreachable!("rule should be expr or decl"),
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
        BinaryExpr(op.as_str().trim().to_owned(), Box::new(lhs), Box::new(rhs))
    })
}

fn parse_binary_operand(node: Pair<Rule>) -> Expr {
    match node.as_rule() {
        Rule::expr_unary => parse_expr_unary(node),
        Rule::expr_relational => parse_expr_binary(node),
        Rule::expr_binary_level1 => parse_expr_binary(node),
        Rule::expr_binary_level2 => parse_expr_binary(node),
        Rule::expr_binary_level3 => parse_expr_binary(node),
        _ => unreachable!("unsatisfied binary expr operands"),
    }
}

fn parse_expr_unary(node: Pair<Rule>) -> Expr {
    let mut nodes: VecDeque<Pair<Rule>> = node.into_inner().into_iter().collect();
    let first = nodes.pop_front().unwrap();
    match first.as_rule() {
        Rule::unary_op => {
            assert_eq!(nodes.len(), 1);
            let operand = parse_expr_unary(nodes.pop_back().unwrap());
            UnaryExpr(first.as_str().trim().to_owned(), Box::new(operand))
        }
        Rule::expr_atom => {
            let primary = parse_expr_atom(first);
            nodes
                .into_iter()
                .flat_map(|apply| apply.into_inner())
                .fold(primary, |lhs, arg| {
                    ApplyExpr(Box::new(lhs), Box::new(parse_expr(arg)))
                })
        }
        _ => unreachable!("expr unary inner should be expr_primary or unary_op"),
    }
}

fn parse_expr_atom(node: Pair<Rule>) -> Expr {
    let child = node.into_inner().next();
    match child {
        None => Expr::Unit,
        Some(child) => match child.as_rule() {
            Rule::expr => parse_expr(child),
            Rule::expr_lambda => parse_lambda(child),
            Rule::expr_match => parse_match(child),
            Rule::id => AtomExpr(AtomId(child.as_str().to_owned())),
            Rule::literal => AtomExpr(AtomLit(parse_lit(child))),
            _ => {
                unreachable!("expr primary inner should be expr_quoted, expr_lambda, id or literal")
            }
        },
    }
}

fn parse_lambda(node: Pair<Rule>) -> Expr {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::normal_lambda => parse_normal_lambda(child),
        Rule::quick_lambda => parse_quick_lambda(child),
        _ => unreachable!("lambda inner should be normal_lambda or quick_lambda"),
    }
}

fn parse_normal_lambda(node: Pair<Rule>) -> Expr {
    let mut nodes: VecDeque<Pair<Rule>> = node.into_inner().into_iter().collect();
    let params: Vec<Name> = nodes
        .pop_front()
        .unwrap()
        .into_inner()
        .into_iter()
        .map(|id| id.as_str().to_owned())
        .collect();
    let body = parse_expr_list(nodes.pop_back().unwrap());
    AtomExpr(AtomRawLambda(params, body))
}

fn parse_quick_lambda(node: Pair<Rule>) -> Expr {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::logical_op => (),
        Rule::relational_op => (),
        Rule::level1_op => (),
        Rule::level2_op => (),
        Rule::level3_op => (),
        _ => unreachable!("unsupported quick lambda operator: {}", child.as_str()),
    }
    let body = BinaryExpr(
        child.as_str().trim().to_owned(),
        Box::new(DBI(0)),
        Box::new(DBI(1)),
    );
    let lam = AtomLambda(2, 0, vec![body]);
    AtomExpr(lam)
}

fn parse_match(node: Pair<Rule>) -> Expr {
    let mut iter = node.into_inner().into_iter();

    let match_expr_node = iter.next().unwrap();
    let match_expr = parse_expr(match_expr_node);

    MatchExpr(Box::new(match_expr), parse_match_case(iter))
}

fn parse_match_case(match_cases: Pairs<Rule>) -> Vec<MatchCase> {
    match_cases
        .into_iter()
        .map(|case| {
            let mut iter = case.into_inner();
            let pattern = iter.next().unwrap();
            let result = iter.next().unwrap();
            MatchCase(parse_pattern(pattern), parse_expr(result))
        })
        .collect()
}

fn parse_pattern(pat: Pair<Rule>) -> Pattern {
    match pat.into_inner().next() {
        Some(non_wildcard) => match non_wildcard.as_rule() {
            Rule::literal => PatLit(parse_lit(non_wildcard)),
            Rule::pat_enum_variant => PatVariant(parse_pat_enum_variant(non_wildcard)),
            _ => unreachable!("internal error: invalid pattern"),
        },
        _ => PatWildcard,
    }
}

fn parse_expr_list(node: Pair<Rule>) -> Vec<Expr> {
    node.into_inner()
        .into_iter()
        .map(|expr| parse_expr(expr))
        .collect()
}

fn parse_lit(lit: Pair<Rule>) -> Lit {
    let lit = lit.into_inner().next().unwrap();
    match lit.as_rule() {
        Rule::number_lit => LitNumber(lit.as_str().parse::<f64>().unwrap()),
        Rule::string_lit => LitString(lit.as_str().to_owned()),
        Rule::bool_lit => LitBool(lit.as_str().parse::<bool>().unwrap()),
        _ => unreachable!("unsupported literal type: {:?}", lit.as_rule()),
    }
}

fn parse_decl(node: Pair<Rule>) -> Decl {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::let_decl => parse_let_decl(child),
        Rule::enum_decl => parse_enum_decl(child),
        _ => unreachable!("unsupported decl type: {:?}", child.as_rule()),
    }
}

fn parse_let_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let expr = parse_expr(iter.next().unwrap());
    LetDecl(id.to_owned(), expr)
}

fn parse_enum_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let variants = iter.map(parse_enum_variant).collect();
    EnumDecl(id.to_owned(), variants)
}

fn parse_enum_variant(node: Pair<Rule>) -> EnumVariant {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    EnumVariant {
        name: id.to_owned(),
        fields: iter.count() as i32,
    }
}

fn parse_pat_enum_variant(node: Pair<Rule>) -> PatEnumVariant {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    PatEnumVariant {
        name: id.to_owned(),
        fields: iter.map(|field| field.as_str().to_owned()).collect(),
    }
}

fn dbi_lambda(param_stack: &mut VecDeque<&Vec<Name>>, expr: Expr) -> Expr {
    match expr {
        // if this is a unsolved lambda
        AtomExpr(AtomRawLambda(names, body)) => dbi_lambda_body(param_stack, names, body),

        ApplyExpr(f, a) => ApplyExpr(
            Box::new(dbi_lambda(param_stack, *f.clone())),
            Box::new(dbi_lambda(&mut VecDeque::new(), *a.clone())),
        ),

        // not a lambda, just return what we have now
        _ => expr,
    }
}

fn dbi_lambda_body(
    param_stack: &mut VecDeque<&Vec<Name>>,
    names: Vec<Name>,
    body: Vec<Expr>,
) -> Expr {
    // I know what I am doing!
    unsafe {
        // This is totally SAFE!!!!
        let ptr: *const Vec<Name> = &names;
        param_stack.push_front(&*ptr);
    }

    // recursively convert variable name to dbi
    let r = AtomExpr(AtomLambda(
        names.len() as i32,
        0,
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
        AtomExpr(AtomId(id)) => {
            if let Some(index) = resolve_param(param_stack, id.as_str()) {
                DBI(index)
            } else {
                expr
            }
        }

        UnaryExpr(op, unary) => {
            UnaryExpr(op.clone(), Box::new(dbi_expr(param_stack, *unary.clone())))
        }

        BinaryExpr(op, lhs, rhs) => BinaryExpr(
            op.clone(),
            Box::new(dbi_expr(param_stack, *lhs.clone())),
            Box::new(dbi_expr(param_stack, *rhs.clone())),
        ),

        ApplyExpr(f, a) => ApplyExpr(
            Box::new(dbi_expr(param_stack, *f.clone())),
            Box::new(dbi_expr(param_stack, *a.clone())),
        ),

        MatchExpr(matchee, cases) => MatchExpr(
            Box::new(dbi_expr(param_stack, *matchee.clone())),
            cases
                .into_iter()
                .map(|MatchCase(pat, expr)| {
                    MatchCase(pat.clone(), dbi_expr(param_stack, expr.clone()))
                })
                .collect(),
        ),

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
