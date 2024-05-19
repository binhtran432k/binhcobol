use std::env;

use xtask::gen;
use xtask::help;
use xtask::DynError;

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{}", e);
        std::process::exit(-1);
    }
}

fn try_main() -> Result<(), DynError> {
    let task = env::args().nth(1);
    match task.as_deref() {
        Some("codegen") => gen::codegen()?,
        _ => help::print_help(),
    }
    Ok(())
}
