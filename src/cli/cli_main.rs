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
    history_file: Option<String>,
}

impl REPL {
    fn new() -> REPL {
        let history_file = dirs::home_dir()
            .map(|mut path| {
                path.push(".fs-history");
                path.to_str().map(|s| s.to_owned())
            })
            .flatten();

        let mut repl = REPL {
            context: Context::new(),
            rl: Editor::<()>::new(),
            history_file,
        };

        if let Some(ref path) = repl.history_file {
            let _ = repl.rl.load_history(path);
        }

        repl
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

impl Drop for REPL {
    fn drop(&mut self) {
        if let Some(ref path) = self.history_file {
            let _ = self.rl.save_history(path);
        }
    }
}

pub(crate) fn cli_main() {
    let mut repl = REPL::new();
    repl.start();
}
