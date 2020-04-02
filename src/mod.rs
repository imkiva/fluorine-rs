pub mod cli;
pub mod gui;

fn main() {
    cli::cli_main::cli_main();
    gui::gui_main::gui_main();
}
