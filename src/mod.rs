pub mod cli;
pub mod gui;

use clap::App;

fn main() {
    let args = App::new("flourine")
        .version("0.1.0")
        .author("Kiva <imkiva@icloud.com>")
        .about("Fluorine interactive math")
        .args_from_usage(
            "-i, --repl        'Starts an interactive fluorine shell'")
        .get_matches();

    match args.is_present("repl") {
        true => cli::cli_main::cli_main(),
        _ => gui::gui_main::gui_main(),
    }
}
