use lang::{
    runtime::Context,
    syntax::{
        optimize::Optimizer,
        parse::{CompileError, FsParser},
        tree::Program,
    },
};

use crate::config::Config;
use rustyline::{error::ReadlineError, Editor};

struct REPL {
    context: Context,
    rl: Editor<()>,
    history_file: Option<String>,
    cfg: Config,
    repl_run: bool,
    prompt: String,
    multiline: bool,
    multiline_buffer: Vec<String>,
}

impl REPL {
    fn new(cfg: Config) -> REPL {
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
            cfg,
            repl_run: true,
            prompt: "Fs> ".to_owned(),
            multiline: false,
            multiline_buffer: Vec::new(),
        };

        if let Some(ref path) = repl.history_file {
            let _ = repl.rl.load_history(path);
        }

        repl
    }

    fn start(&mut self) {
        while self.repl_run {
            let readline = self.rl.readline(self.prompt.as_str());
            match readline {
                Ok(line) => {
                    if line.starts_with(":") {
                        self.process_command(line);
                        continue;
                    }

                    match self.multiline {
                        true => self.multiline_buffer.push(line),
                        _ => self.run_code(line),
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

    fn process_command(&mut self, line: String) {
        match line.as_str() {
            ":{" => {
                self.multiline = true;
                self.prompt = "Fs| ".to_owned();
            }

            ":}" => {
                self.multiline = false;
                let line = self.multiline_buffer.join("\n");
                self.run_code(line);
            }

            ":q" => {
                self.repl_run = false;
            }

            ":scope" => {
                let scope = self.context.stack.front().expect("?");
                println!("Enums: ");
                scope.enums.iter().for_each(|(k, _)| println!("- {}", k));
                println!();
                println!("Vars:");
                scope.vars.iter().for_each(|(k, _)| println!("- {}", k));
                println!();
            }

            _ => println!("REPL: Unknown command {}", line.as_str()),
        }
    }

    fn run_code(&mut self, line: String) {
        self.rl.add_history_entry(line.as_str());
        compile_and_run(&self.cfg, &mut self.context, "<stdin>", line.as_str());
    }
}

impl Drop for REPL {
    fn drop(&mut self) {
        if let Some(ref path) = self.history_file {
            let _ = self.rl.save_history(path);
        }
    }
}

fn compile(cfg: &Config, input: &str) -> Result<Program, CompileError> {
    FsParser::ast(input).map(|ast| Optimizer::run(ast, cfg.opt_level.clone()))
}

fn compile_and_run(cfg: &Config, ctx: &mut Context, file: &str, input: &str) {
    match compile(cfg, input) {
        Ok(tree) => {
            if cfg.dump_ast {
                eprintln!("{:#?}", tree);
            }

            match ctx.source(tree) {
                Ok(v) => println!("{}", v),
                Err(err) => eprintln!("{}", err),
            }
        }
        Err(CompileError(e)) => eprintln!("{}", e.with_path(file)),
    }
}

pub(crate) fn cli_main(cfg: Config, input: Option<String>) {
    if let Some(input) = input {
        let src = std::fs::read_to_string(input.as_str()).expect("unable to open file");
        let mut ctx = Context::new();
        compile_and_run(&cfg, &mut ctx, input.as_str(), src.as_str());
    } else {
        let mut repl = REPL::new(cfg);
        repl.start();
    }
}
