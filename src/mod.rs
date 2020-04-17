pub mod cli;
pub mod gui;
pub mod config;

use clap::App;
use script::optimize::OptimizeLevel;

fn main() {
    let args = App::new("fluorine")
        .version("0.1.0")
        .author("Kiva <imkiva@icloud.com>")
        .about("Fluorine interactive math")
        .args_from_usage("\
            -i, --repl               'Starts an interactive fluorine shell'
            -g, --gui                'Starts an GUI'
            -d, --dump-ast           'Prints AST before evaluation'
            -O, --optimize=<level>   'Sets optimization level (0-3)'
            [INPUT]                  'Source code to run'")
        .get_matches();

    let mut cfg = config::Config::new();
    if let Some(value) = args.value_of("optimize") {
        match value {
            "0" => cfg.opt_level = OptimizeLevel::Disabled,
            "1" => cfg.opt_level = OptimizeLevel::Normal,
            "2" => cfg.opt_level = OptimizeLevel::Aggressive,
            "3" => cfg.opt_level = OptimizeLevel::JustDoIt,
            _ => (),
        }
    }

    if args.is_present("dump-ast") {
        cfg.dump_ast = true;
    }

    let input = args.value_of("INPUT");

    if args.is_present("repl") || input.is_none() {
        cli::cli_main::cli_main(cfg, None);
    } else if args.is_present("gui") {
        gui::gui_main::gui_main();
    } else {
        cli::cli_main::cli_main(cfg, input.map(|e| e.to_owned()));
    }
}
