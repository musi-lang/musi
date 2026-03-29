mod format;
mod parse;

pub use format::{disassemble_method, format_instruction};
pub use parse::{assemble_method, parse_instruction};

#[cfg(test)]
mod tests;
