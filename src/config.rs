use script::optimize::OptimizeLevel;

pub struct Config {
    pub opt_level: OptimizeLevel,
    pub dump_ast: bool,
}

impl Config {
    pub(crate) fn new() -> Self {
        Config {
            opt_level: OptimizeLevel::Aggressive,
            dump_ast: false,
        }
    }
}
