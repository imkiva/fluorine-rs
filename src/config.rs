use lang::syntax::optimize::OptimizeLevel;

pub struct Config {
    pub opt_level: OptimizeLevel,
    pub dump_ast: bool,
}

impl Config {
    pub(crate) fn new() -> Self {
        Config {
            opt_level: OptimizeLevel::Normal,
            dump_ast: false,
        }
    }
}
