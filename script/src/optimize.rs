use crate::tree::{Program, Expr};
use crate::pe::{PartialEval, PEContext};
use crate::tree::ProgramItem::DeclItem;
use crate::tree::Decl::LetDecl;

use std::collections::HashMap;

pub struct Optimizer;

pub enum OptimizeLevel {
    NONE,
    NORMAL,
    AGGRESSIVE,
}

impl Optimizer {
    pub fn run(input: Program, level: OptimizeLevel) -> Program {
        match level {
            OptimizeLevel::NONE => input,
            OptimizeLevel::NORMAL => input.partial_eval(),
            OptimizeLevel::AGGRESSIVE => {
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

#[cfg(test)]
mod tests {
    use crate::parse::FsParser;
    use crate::optimize::Optimizer;
    use crate::optimize::OptimizeLevel::AGGRESSIVE;

    #[test]
    fn test_aggressive() {
        let ast = FsParser::ast("\
            let a = 1 + 1 + 3\n
            let b = 4 + 5 + 6\n
            let times = { a, b -> a * b }\n
            let result = times(a, b)\n
            let const = { a, b -> a }\n
            let id = { a -> a }\n
            let const_id = const(id)\n
            \
        ").map(|ast| Optimizer::run(ast, AGGRESSIVE));

        if let Ok(p) = ast {
            println!("{:#?}", p);
        } else {
            eprintln!("Internal error")
        }
    }
}
