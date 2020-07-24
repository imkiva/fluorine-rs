use std::{collections::VecDeque, result::Result};

use pest::{
    error::{Error, ErrorVariant},
    iterators::{Pair, Pairs},
    Parser,
};

use crate::syntax::tree::{
    Atom::*,
    Decl::*,
    Expr::*,
    Lit::*,
    Pattern::{PatLit, PatVariant, PatWildcard},
    ProgramItem::*,
    *,
};

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
        Ok(parse_unit(pairs))
    }
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
                .flat_map(|postfix| postfix.into_inner())
                .fold(primary, build_apply_or_member)
        }
        _ => unreachable!("expr unary inner should be expr_primary or unary_op"),
    }
}

fn build_apply_or_member(lhs: Expr, postfix: Pair<Rule>) -> Expr {
    match postfix.as_rule() {
        Rule::postfix_apply => postfix
            .into_inner()
            .into_iter()
            .map(|expr| parse_expr(expr))
            .fold(lhs, |lhs, arg| ApplyExpr(Box::new(lhs), Box::new(arg))),

        Rule::postfix_member => {
            let id = postfix.into_inner().next().unwrap().as_str();
            match id {
                "await" => AwaitExpr(Box::new(lhs)),
                _ => MemberExpr(Box::new(lhs), id.to_string()),
            }
        }

        _ => unreachable!("not a postfix"),
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
            Rule::enum_ctor => AtomExpr(AtomId(child.as_str().to_owned())),
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
    let mut nodes = node.into_inner().into_iter();
    let is_async = match nodes.peek().map(|p| p.as_rule()) {
        Some(Rule::async_token) => {
            let _ = nodes.next().unwrap();
            true
        }
        _ => false,
    };
    let params = parse_param(nodes.next().unwrap());
    let body = parse_expr_list(nodes.next().unwrap());
    AtomExpr(AtomRawLambda(ParseRawLambda {
        is_async,
        params,
        body,
    }))
}

fn parse_param(param: Pair<Rule>) -> Vec<Param> {
    param.into_inner().into_iter().map(parse_id_typed).collect()
}

fn parse_id_typed(id_typed: Pair<Rule>) -> Param {
    let mut nodes = id_typed.into_inner().into_iter();
    let id = nodes.next().unwrap().as_str().to_string();

    let ty = match id.as_str() {
        "self" => Some(ParseType::SelfType),
        _ => nodes.next().map(parse_type),
    };

    Param { id, ty }
}

fn parse_type(node: Pair<Rule>) -> ParseType {
    match node.as_str() {
        "Self" => ParseType::SelfType,
        ty => ParseType::OtherType(ty.to_string()),
    }
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
    let lam = AtomLambda(
        vec![
            Param {
                id: "_1".to_string(),
                ty: Some(ParseType::OtherType("// TODO: <OP-TRAIT>".to_string())),
            },
            Param {
                id: "_2".to_string(),
                ty: Some(ParseType::OtherType("// TODO: <OP-TRAIT>".to_string())),
            },
        ],
        0,
        vec![body],
    );
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

fn parse_pat_enum_variant(node: Pair<Rule>) -> PatEnumVariant {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    PatEnumVariant {
        name: id.to_owned(),
        fields: iter.map(|field| field.as_str().to_owned()).collect(),
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
        Rule::string_lit => {
            let s = lit.as_str().to_owned();
            let s = s[1..s.len() - 1].into();
            LitString(unescaped(s))
        }
        Rule::bool_lit => LitBool(lit.as_str().parse::<bool>().unwrap()),
        _ => unreachable!("unsupported literal type: {:?}", lit.as_rule()),
    }
}

fn unescaped(input: &str) -> String {
    let mut str = String::with_capacity(input.len());
    let mut escape = false;
    for ch in input.chars() {
        if escape {
            escape = false;
            str.push(unescaped_char(ch));
        } else {
            match ch {
                '\\' => escape = true,
                _ => str.push(ch),
            }
        }
    }
    str
}

fn unescaped_char(ch: char) -> char {
    match ch {
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        'a' => '\u{07}',
        'b' => '\u{08}',
        'f' => '\u{0C}',
        'v' => '\u{0B}',
        '0' => '\0',
        '\'' => '\'',
        '\"' => '\"',
        '\\' => '\\',
        _ => ch,
    }
}

fn parse_decl(node: Pair<Rule>) -> Decl {
    let child = node.into_inner().next().unwrap();
    match child.as_rule() {
        Rule::let_decl => parse_let_decl(child),
        Rule::enum_decl => parse_enum_decl(child),
        Rule::trait_decl => parse_trait_decl(child),
        Rule::impl_decl => parse_impl_decl(child),
        _ => unreachable!("unsupported decl type: {:?}", child.as_rule()),
    }
}

fn peek_and_parse_generic(iter: &mut Pairs<Rule>) -> Vec<GenericParam> {
    match iter.peek().map(|it| it.as_rule()) {
        Some(Rule::generic_param) => parse_generic_param(iter.next().unwrap()),
        _ => vec![],
    }
}

fn parse_generic_param(node: Pair<Rule>) -> Vec<GenericParam> {
    node.into_inner()
        .into_iter()
        .map(parse_constrained)
        .map(|(param, constraints)| GenericParam {
            name: param,
            constraints,
        })
        .collect()
}

fn parse_constrained(node: Pair<Rule>) -> (Ident, Vec<Constraint>) {
    let mut iter = node.into_inner().into_iter();
    let param = iter.next().unwrap().as_str();
    let constraints = iter
        .map(|ty| ty.as_str().to_string())
        .map(Constraint::MustImpl)
        .collect();
    (param.to_string(), constraints)
}

fn parse_let_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let expr = parse_expr(iter.next().unwrap());
    LetDecl(id.to_owned(), expr)
}

fn parse_enum_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let type_ = iter.next().unwrap().as_str();
    let generic = peek_and_parse_generic(&mut iter);
    let variants = iter.map(parse_enum_variant).collect();

    EnumDecl(generic, type_.to_owned(), variants)
}

fn parse_enum_variant(node: Pair<Rule>) -> EnumVariant {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    EnumVariant {
        name: id.to_owned(),
        field_types: iter.map(|ty| ty.as_str().to_string()).collect(),
    }
}

fn parse_trait_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let fns = iter.map(parse_trait_fn).collect();
    TraitDecl(id.to_owned(), fns)
}

fn parse_trait_fn(node: Pair<Rule>) -> TraitFn {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let param = parse_param(iter.next().unwrap());
    let ret = parse_type(iter.next().unwrap());
    TraitFn {
        name: id.to_owned(),
        param,
        ret,
    }
}

fn parse_impl_decl(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let generic = peek_and_parse_generic(&mut iter);
    let tr = iter.next().unwrap().as_str();
    let ty = iter.next().unwrap().as_str();
    let fns = iter.map(parse_impl_fn).collect();
    ImplDecl(generic, tr.to_owned(), ty.to_owned(), fns)
}

fn parse_impl_fn(node: Pair<Rule>) -> Decl {
    let mut iter = node.into_inner().into_iter();
    let id = iter.next().unwrap().as_str();
    let lambda = parse_normal_lambda(iter.next().unwrap());
    LetDecl(id.to_owned(), lambda)
}
