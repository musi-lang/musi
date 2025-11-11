#![no_std]

extern crate alloc;

pub mod frame;
pub mod instr;
pub mod loader;
pub mod stack;
pub mod types;
pub mod value;
pub mod vm;

fn main() {
    unsafe {
        // TODO: parse CLI args
        // TODO: load .msc file
        // TODO: exec VM
    }
}
