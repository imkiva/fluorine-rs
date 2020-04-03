use crate::tree::Program;
use crate::pe::PartialEval;

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
                input.partial_eval()
            },
        }
    }
}
