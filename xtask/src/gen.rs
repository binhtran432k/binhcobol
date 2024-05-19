mod ast_src;
mod grammar;

use crate::DynError;

pub fn codegen() -> Result<(), DynError> {
    grammar::generate();
    Ok(())
}
