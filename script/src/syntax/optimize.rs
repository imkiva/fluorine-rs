use crate::syntax::{
    pe::{PEContext, PartialEval},
    tree::{Decl::LetDecl, Expr, Program, ProgramItem::DeclItem},
};

use std::collections::HashMap;

pub struct Optimizer;

#[derive(Clone)]
pub enum OptimizeLevel {
    Disabled,
    Normal,
    Aggressive,
    JustDoIt,
}

impl Optimizer {
    pub fn run(input: Program, level: OptimizeLevel) -> Program {
        match level {
            OptimizeLevel::Disabled => input,
            OptimizeLevel::Normal => input.partial_eval(),
            OptimizeLevel::Aggressive | OptimizeLevel::JustDoIt => {
                let mut ctx = OptimizeContext::new();
                ctx.prepare(&input);
                input.partial_eval_with(Some(&ctx))
            }
        }
    }
}

struct OptimizeContext {
    vars: HashMap<String, Expr>,
}

impl OptimizeContext {
    fn new() -> OptimizeContext {
        OptimizeContext {
            vars: Default::default(),
        }
    }

    fn prepare(&mut self, program: &Program) {
        program.into_iter().for_each(|item| match item {
            DeclItem(LetDecl(name, expr)) => {
                self.vars.insert(name.clone(), expr.clone());
            }
            _ => (),
        });
    }
}

impl PEContext for OptimizeContext {
    fn try_resolve_constant(&self, name: &str) -> Option<Expr> {
        self.vars.get(name).map(|e| e.clone())
    }
}
