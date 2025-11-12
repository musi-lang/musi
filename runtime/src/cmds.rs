use crate::{binary, loader, vm::VM};
use std::fs;

pub unsafe fn run(file: &str) {
    let bc = loader::load(file);
    let mut vm = VM::new(bc);
    vm.exec();
}

pub fn disasm(file: &str) {
    let bytes = fs::read(file).expect("failed to read file");
    let mut pos = 24;
    let code_offset = binary::read_u32_le(&bytes, &mut pos) as usize;

    println!(".procedure export main() {{");

    let mut pos = code_offset;
    while pos < bytes.len() {
        let opcode = bytes[pos];
        print!("  IL_{pos:04x}: ");
        pos += 1;

        match opcode {
            0x00 => println!("nop"),
            0x10 => {
                let idx = binary::read_u32_le(&bytes, &mut pos);
                println!("ld.c {idx}");
            }
            0x11 => {
                let val = binary::read_i32_le(&bytes, &mut pos);
                println!("ld.c.i4 {val}");
            }
            0x20 => {
                let idx = binary::read_u32_le(&bytes, &mut pos);
                println!("ld.loc {idx}");
            }
            0x21 => {
                let idx = binary::read_u32_le(&bytes, &mut pos);
                println!("st.loc {idx}");
            }
            0x70 => {
                let idx = binary::read_u32_le(&bytes, &mut pos);
                println!("call {idx}");
            }
            0x72 => println!("ret"),
            _ => println!("unknown 0x{:02x}", opcode),
        }
    }

    println!("}}");
}
