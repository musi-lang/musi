use crate::vm::VM;

extern crate alloc;

pub mod binary;
pub mod frame;
pub mod instr;
pub mod loader;
pub mod stack;
pub mod types;
pub mod value;
pub mod vm;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} <file.ms>", args[0]);
        std::process::exit(1);
    }

    let bc = unsafe { loader::load(&args[1]) };
    let mut vm = VM::new(bc);
    vm.exec();
}
