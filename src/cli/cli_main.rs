use script::parser::FsParser;
use script::parser::CompileError;
use script::tree::Program;
use script::optimizer::Optimizer;

use rustyline::error::ReadlineError;
use rustyline::Editor;

pub(crate) fn cli_main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("F> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match compile_line(line.as_str()) {
                    Ok(tree) => println!("{:#?}", tree),
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
    FsParser::ast(input).map(Optimizer::run)
}
