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
                    compile_and_run(&mut self.context,
                                    "<stdin>", line.as_str());
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
}

impl Drop for REPL {
    fn drop(&mut self) {
        if let Some(ref path) = self.history_file {
            let _ = self.rl.save_history(path);
        }
    }
}

fn compile(input: &str) -> Result<Program, CompileError> {
    FsParser::ast(input).map(
        |ast| Optimizer::run(ast,
                             OptimizeLevel::AGGRESSIVE))
}

fn compile_and_run(ctx: &mut Context, file: &str, input: &str) {
    match compile(input) {
        Ok(tree) => match ctx.source(tree) {
            Ok(Some(v)) => println!("{}", v),
            Ok(None) => (),
            Err(err) => eprintln!("{}", err),
        },
        Err(CompileError(e)) => eprintln!("{}", e.with_path(file)),
    }
}

pub(crate) fn cli_main(input: Option<String>) {
    if let Some(input) = input {
        let src = std::fs::read_to_string(input.as_str()).expect("unable to open file");
        let mut ctx = Context::new();
        compile_and_run(&mut ctx, input.as_str(), src.as_str());
    } else {
        let mut repl = REPL::new();
        repl.start();
    }
}
