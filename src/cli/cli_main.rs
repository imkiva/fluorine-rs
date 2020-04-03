use script::parse::FsParser;
use script::parse::CompileError;
use script::tree::Program;
use script::optimize::{Optimizer, OptimizeLevel};
use script::eval::Context;

use rustyline::error::ReadlineError;
use rustyline::Editor;

pub(crate) fn cli_main() {
    let mut ctx = Context::new();
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline("Fs> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match compile_line(line.as_str()) {
                    Ok(tree) => match ctx.source(tree) {
                        Ok(Some(v)) => println!("{}", v),
                        Ok(None) => (),
                        Err(err) => eprintln!("{}", err),
                    },
                    Err(CompileError(e)) => eprintln!("{}", e.with_path("<stdin>")),
                }
            }

            Err(ReadlineError::Interrupted) => (),
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("ReadlineError: {:?}", err);
                break;
            }
        }
    }
}

fn compile_line(input: &str) -> Result<Program, CompileError> {
    FsParser::ast(input).map(|ast|
        Optimizer::run(ast, OptimizeLevel::AGGRESSIVE))
}
