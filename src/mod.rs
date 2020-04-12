pub mod cli;
pub mod gui;

use clap::App;

fn main() {
    let args = App::new("fluorine")
        .version("0.1.0")
        .author("Kiva <imkiva@icloud.com>")
        .about("Fluorine interactive math")
        .args_from_usage(
            "-i, --repl        'Starts an interactive fluorine shell'
            -g, --gui         'Starts an GUI'
            <INPUT>           'Run file'")
        .get_matches();

    if args.is_present("repl") {
        cli::cli_main::cli_main(None);
    } else if args.is_present("gui") {
        gui::gui_main::gui_main();
    } else {
        cli::cli_main::cli_main(args.value_of("INPUT").map(|e| e.to_owned()));
    }
}
