use rustyline::{
    completion::{Candidate, Completer},
    error::ReadlineError,
    highlight::Highlighter,
    hint::Hinter,
    validate::Validator,
    Context as RContext, Editor, Helper, Result as RResult,
};

use lang::{
    runtime::Context,
    syntax::{
        optimize::Optimizer,
        parse::{CompileError, FsParser},
        tree::Program,
    },
};

use crate::config::Config;
use lang::runtime::Value;

struct REPL {
    rl: Editor<REPLHelper>,
    history_file: Option<String>,
    cfg: Config,
    repl_run: bool,
    prompt: String,
    multiline: bool,
    multiline_buffer: Vec<String>,
}

enum CompleteCandidate {
    Command(String, String),
    Var(String),
}

impl Candidate for CompleteCandidate {
    fn display(&self) -> &str {
        match self {
            CompleteCandidate::Command(cmd, _) => cmd,
            CompleteCandidate::Var(name) => name,
        }
    }

    fn replacement(&self) -> &str {
        match self {
            CompleteCandidate::Command(cmd, _) => cmd,
            CompleteCandidate::Var(name) => name,
        }
    }
}

struct REPLHelper {
    context: Context,
}

impl Helper for REPLHelper {}

impl Completer for REPLHelper {
    type Candidate = CompleteCandidate;

    fn complete(
        &self,
        line: &str,
        _pos: usize,
        _ctx: &RContext<'_>,
    ) -> RResult<(usize, Vec<Self::Candidate>)> {
        if line.starts_with(":") {
            let commands = vec![
                (":q", "Quit REPL"),
                (":{", "Start multi-line mode"),
                (":}", "End multi-line mode"),
                (":scope", "Show current scope vars and enums"),
            ];
            Ok((
                line.len(),
                commands
                    .into_iter()
                    .filter(|it| it.0.starts_with(line))
                    .map(|it| (it.0.split_at(line.len()).1, it.1))
                    .map(|it| CompleteCandidate::Command(it.0.to_owned(), it.1.to_owned()))
                    .collect(),
            ))
        } else {
            Ok((
                line.len(),
                self.context
                    .stack
                    .iter()
                    .flat_map(|it| it.vars.keys())
                    .map(|it| it.split_at(line.len()).1)
                    .map(|it| CompleteCandidate::Var(it.to_owned()))
                    .collect(),
            ))
        }
    }
}

impl Validator for REPLHelper {}

impl Hinter for REPLHelper {}

impl Highlighter for REPLHelper {}

impl REPL {
    fn new(cfg: Config) -> REPL {
        let history_file = dirs::home_dir()
            .map(|mut path| {
                path.push(".fs-history");
                path.to_str().map(|s| s.to_owned())
            })
            .flatten();

        let mut repl = REPL {
            rl: Editor::<REPLHelper>::new(),
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

        let mut helper = REPLHelper {
            context: Context::new(),
        };

        helper.context.load_builtins();
        repl.rl.set_helper(Some(helper));
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
                self.prompt = "Fs> ".to_owned();
                let line = self.multiline_buffer.join("\n");
                self.run_code(line);
            }

            ":q" => {
                self.repl_run = false;
            }

            ":scope" => {
                let scope = self.rl.helper().unwrap().context.stack.front().expect("?");
                println!("Enums: ");
                scope.enums.iter().for_each(|(k, _)| println!("- {}", k));
                println!();
                println!("Traits: ");
                scope.traits.iter().for_each(|(k, _)| println!("- {}", k));
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
        compile_and_run(
            true,
            &self.cfg,
            &mut self.rl.helper_mut().unwrap().context,
            "<stdin>",
            line.as_str(),
        );
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

fn compile_and_run(show_unit: bool, cfg: &Config, ctx: &mut Context, file: &str, input: &str) {
    match compile(cfg, input) {
        Ok(tree) => {
            if cfg.dump_ast {
                eprintln!("{:#?}", tree);
            }

            match ctx.source(tree) {
                Ok(Value::UnitValue) => {
                    if show_unit {
                        println!("{}", Value::UnitValue);
                    }
                }
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
        ctx.load_builtins();
        compile_and_run(false, &cfg, &mut ctx, input.as_str(), src.as_str());
    } else {
        let mut repl = REPL::new(cfg);
        repl.start();
    }
}
