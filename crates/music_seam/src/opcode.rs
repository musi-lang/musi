mod model;
mod table;

pub use model::{Opcode, OpcodeFamily, OpcodeWire};
#[cfg(test)]
use table::{opcode_info_count, opcode_infos};

#[cfg(test)]
mod tests;
