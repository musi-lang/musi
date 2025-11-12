extern crate alloc;

pub mod stdlib;

mod builtin;
mod exec;
mod frame;
mod link;
mod memory;
mod parse;
mod stack;
mod string;
mod types;

use exec::{vm_drop, vm_exec, vm_new, disasm_instr};
use parse::parse_module;
use stdlib::init_vm_with_stdlib;
use string::string_new;

use link::link_modules;
use memory::mem_alloc;

pub unsafe fn print_usage() {
    libc::printf(b"usage: musi <run|disasm> <file>\n\0".as_ptr() as *const i8);
}

pub unsafe fn run_file(filename: &str) {
    let c_filename = string_new(filename);

    let file = libc::fopen(c_filename as *const i8, b"rb\0".as_ptr() as *const i8);

    if file.is_null() {
        libc::printf(
            b"failed to open file '%s'\n\0".as_ptr() as *const i8,
            c_filename as *const i8,
        );
        libc::exit(1);
    }

    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::fseek(file, 0, libc::SEEK_SET);

    let data = mem_alloc(size);
    libc::fread(data as *mut libc::c_void, 1, size, file);
    libc::fclose(file);

    let data_slice = alloc::slice::from_raw_parts(data, size);
    let (consts, procs, code) = parse_module(data_slice);

    let bytecode = link_modules(vec![consts], vec![procs], vec![code]);

    let mut vm = vm_new(&bytecode);
    let vm_ptr = &mut vm as *mut _;

    if !init_vm_with_stdlib(vm_ptr) {
        libc::printf(b"failed to ready stdlib\n\0".as_ptr() as *const i8);
        libc::exit(1);
    }

    vm_exec(vm_ptr);
    vm_drop(vm);
}

pub unsafe fn disasm_file(filename: &str) {
    let c_filename = string_new(filename);

    let file = libc::fopen(c_filename as *const i8, b"rb\0".as_ptr() as *const i8);

    if file.is_null() {
        libc::printf(
            b"failed to open file '%s'\n\0".as_ptr() as *const i8,
            c_filename as *const i8,
        );
        libc::exit(1);
    }

    libc::fseek(file, 0, libc::SEEK_END);
    let size = libc::ftell(file) as usize;
    libc::fseek(file, 0, libc::SEEK_SET);

    let data = mem_alloc(size);
    libc::fread(data as *mut libc::c_void, 1, size, file);
    libc::fclose(file);

    let data_slice = alloc::slice::from_raw_parts(data, size);
    let (consts, procs, code) = parse_module(data_slice);

    let bytecode = link_modules(vec![consts], vec![procs], vec![code]);

    libc::printf(b"=== Disassembly of %s ===\n\0".as_ptr() as *const i8, c_filename as *const i8);

    if !bytecode.code.is_null() {
        let code = bytecode.code;
        let code_ptr = (*code).as_ptr();
        let mut ip = 0;
        let mut instr_count = 0;

        while ip < (*code).len() {
            libc::printf(
                b"%04x: %s\n\0".as_ptr() as *const i8,
                instr_count,
                disasm_instr(code_ptr, &mut ip).as_ptr() as *const i8,
            );
            instr_count += 1;

            // Limit output to prevent overwhelming
            if instr_count >= 100 {
                libc::printf(b"... (truncated after 100 instructions)\n\0".as_ptr() as *const i8);
                break;
            }
        }

        libc::printf(b"\nTotal instructions: %d\n\0".as_ptr() as *const i8, instr_count);
    } else {
        libc::printf(b"No code section found\n\0".as_ptr() as *const i8);
    }
}

pub fn main() {
    // In Crust-compliant version, we'd need to use `clap` crate without `std` feature.
    // For now, simple argument parsing
    unsafe {
        let args = libc::sysconf(libc::_SC_ARG_MAX);
        if args >= 2 {
            // For now, default to running test file
            // TODO: proper CLI argument parsing
            run_file("examples/hello.msc");
        } else {
            print_usage();
            libc::exit(1);
        }
    }
}
