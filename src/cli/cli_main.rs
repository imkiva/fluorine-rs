use script::parse::FsParser;
use script::parse::CompileError;
use script::tree::Program;
use script::optimize::{Optimizer, OptimizeLevel};
use script::eval::Context;

use rustyline::error::ReadlineError;
use rustyline::Editor;

struct REPL {
    context: Context,
    rl: Editor<()>,
}

impl REPL {
    fn new() -> REPL {
        REPL {
            context: Context::new(),
            rl: Editor::<()>::new(),
        }
    }

    fn start(&mut self) {
        loop {
            let readline = self.rl.readline("Fs> ");
            match readline {
                Ok(line) => {
                    self.rl.add_history_entry(line.as_str());
                    match self.compile_line(line.as_str()) {
                        Ok(tree) => match self.context.source(tree) {
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

    fn compile_line(&mut self, input: &str) -> Result<Program, CompileError> {
        FsParser::ast(input).map(
            |ast| Optimizer::run(ast,
                                 OptimizeLevel::AGGRESSIVE))
    }
}

pub(crate) fn cli_main() {
    let mut repl = REPL::new();
    repl.start();
}
