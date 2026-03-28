#![allow(clippy::unwrap_used)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use musi_vm::internal::{Method, Module};
use musi_vm::Vm;
use music_il::opcode::Opcode;

const ENTRY_POINT_NAME: u32 = u32::MAX;

const fn op(o: Opcode) -> u8 {
    o as u8
}

fn module_with_method(locals: u16, code: Vec<u8>) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: locals,
            code,
        }],
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    }
}

fn bench_arithmetic_loop(c: &mut Criterion) {
    let mut group = c.benchmark_group("dispatch_arith");
    for n in [1_000i16, 10_000, 30_000] {
        let _ = group.bench_with_input(BenchmarkId::new("iadd_loop", n), &n, |b, &iters| {
            // Bytecode: accumulator in local 0, counter in local 1
            //   LdNil          ; acc = 0
            //   StLoc 0
            //   LdNil          ; counter = 0
            //   StLoc 1
            // loop:
            //   LdLoc 0
            //   LdOne
            //   IAdd
            //   StLoc 0
            //   LdLoc 1
            //   LdOne
            //   IAdd
            //   StLoc 1
            //   LdLoc 1
            //   LdSmi <iters>
            //   CmpLt
            //   BrTrue <back to loop>
            //   LdLoc 0
            //   Halt
            let iter_bytes = iters.to_le_bytes();
            let lo = iter_bytes[0];
            let hi = iter_bytes[1];
            // After BrTrue reads its i16 operand, pc = 27.
            // Target is offset 6 (loop start), so offset = 6 - 27 = -21.
            let offset: i16 = -21;
            let off_bytes = offset.to_le_bytes();
            let code = vec![
                op(Opcode::LdNil),
                op(Opcode::StLoc),
                0,
                op(Opcode::LdNil),
                op(Opcode::StLoc),
                1,
                // offset 6: loop start
                op(Opcode::LdLoc),
                0,
                op(Opcode::LdOne),
                op(Opcode::IAdd),
                op(Opcode::StLoc),
                0,
                op(Opcode::LdLoc),
                1,
                op(Opcode::LdOne),
                op(Opcode::IAdd),
                op(Opcode::StLoc),
                1,
                op(Opcode::LdLoc),
                1,
                op(Opcode::LdSmi),
                lo,
                hi,
                op(Opcode::CmpLt),
                // offset 23
                op(Opcode::BrTrue),
                off_bytes[0],
                off_bytes[1],
                op(Opcode::LdLoc),
                0,
                op(Opcode::Halt),
            ];
            let module = module_with_method(2, code);
            b.iter(|| {
                let mut vm = Vm::new(module_with_method(2, module.methods[0].code.clone()));
                vm.run().unwrap()
            });
        });
    }
    let _ = group.finish();
}

fn bench_local_load_store(c: &mut Criterion) {
    let mut group = c.benchmark_group("dispatch_locals");
    for n in [1_000i16, 10_000, 30_000] {
        let _ = group.bench_with_input(BenchmarkId::new("load_store_loop", n), &n, |b, &iters| {
            // Bytecode: 4 locals, shuffle values between them N times
            //   LdSmi 1; StLoc 0
            //   LdSmi 2; StLoc 1
            //   LdSmi 3; StLoc 2
            //   LdNil; StLoc 3  (counter)
            // loop:
            //   LdLoc 0; StLoc 1
            //   LdLoc 1; StLoc 2
            //   LdLoc 2; StLoc 0
            //   LdLoc 3; LdOne; IAdd; StLoc 3
            //   LdLoc 3; LdSmi <N>; CmpLt
            //   BrTrue <back>
            //   LdLoc 0; Halt
            let iter_bytes = iters.to_le_bytes();
            let lo = iter_bytes[0];
            let hi = iter_bytes[1];
            // After BrTrue reads its operand, pc = 43.
            // Target is offset 16 (loop start), so offset = 16 - 43 = -27.
            let offset: i16 = -27;
            let off_bytes = offset.to_le_bytes();
            let code = vec![
                op(Opcode::LdOne),
                op(Opcode::StLoc),
                0,
                op(Opcode::LdSmi),
                2,
                0,
                op(Opcode::StLoc),
                1,
                op(Opcode::LdSmi),
                3,
                0,
                op(Opcode::StLoc),
                2,
                op(Opcode::LdNil),
                op(Opcode::StLoc),
                3,
                // offset 16: loop start
                op(Opcode::LdLoc),
                0,
                op(Opcode::StLoc),
                1,
                op(Opcode::LdLoc),
                1,
                op(Opcode::StLoc),
                2,
                op(Opcode::LdLoc),
                2,
                op(Opcode::StLoc),
                0,
                op(Opcode::LdLoc),
                3,
                op(Opcode::LdOne),
                op(Opcode::IAdd),
                op(Opcode::StLoc),
                3,
                op(Opcode::LdLoc),
                3,
                op(Opcode::LdSmi),
                lo,
                hi,
                op(Opcode::CmpLt),
                // offset 37
                op(Opcode::BrTrue),
                off_bytes[0],
                off_bytes[1],
                op(Opcode::LdLoc),
                0,
                op(Opcode::Halt),
            ];
            b.iter(|| {
                let mut vm = Vm::new(module_with_method(4, code.clone()));
                vm.run().unwrap()
            });
        });
    }
    let _ = group.finish();
}

fn bench_heap_array_access(c: &mut Criterion) {
    let mut group = c.benchmark_group("dispatch_heap");
    for n in [100i16, 1_000, 10_000] {
        let _ = group.bench_with_input(BenchmarkId::new("arr_geti_loop", n), &n, |b, &iters| {
            // Bytecode: create array of size 4, loop reading element 0
            //   ArrNew 4         ; local 0 = array
            //   LdSmi 42; ArrSetI 0  ; arr[0] = 42
            //   StLoc 0
            //   LdNil; StLoc 1   ; counter = 0
            // loop:
            //   LdLoc 0; ArrGetI 0; Pop   ; read arr[0], discard
            //   LdLoc 1; LdOne; IAdd; StLoc 1
            //   LdLoc 1; LdSmi <N>; CmpLt
            //   BrTrue <back>
            //   LdLoc 0; ArrGetI 0; Halt
            let iter_bytes = iters.to_le_bytes();
            let lo = iter_bytes[0];
            let hi = iter_bytes[1];
            // After BrTrue reads its operand, pc = 33.
            // Target is offset 13 (loop start), so offset = 13 - 33 = -20.
            let offset: i16 = -20;
            let off_bytes = offset.to_le_bytes();
            let code = vec![
                op(Opcode::ArrNew),
                4,
                0,
                op(Opcode::LdSmi),
                42,
                0,
                op(Opcode::ArrSetI),
                0,
                op(Opcode::StLoc),
                0,
                op(Opcode::LdNil),
                op(Opcode::StLoc),
                1,
                // offset 13: loop start
                op(Opcode::LdLoc),
                0,
                op(Opcode::ArrGetI),
                0,
                op(Opcode::Pop),
                op(Opcode::LdLoc),
                1,
                op(Opcode::LdOne),
                op(Opcode::IAdd),
                op(Opcode::StLoc),
                1,
                op(Opcode::LdLoc),
                1,
                op(Opcode::LdSmi),
                lo,
                hi,
                op(Opcode::CmpLt),
                // offset 29
                op(Opcode::BrTrue),
                off_bytes[0],
                off_bytes[1],
                op(Opcode::LdLoc),
                0,
                op(Opcode::ArrGetI),
                0,
                op(Opcode::Halt),
            ];
            b.iter(|| {
                let mut vm = Vm::new(module_with_method(2, code.clone()));
                vm.run().unwrap()
            });
        });
    }
    let _ = group.finish();
}

criterion_group!(
    dispatch_benches,
    bench_arithmetic_loop,
    bench_local_load_store,
    bench_heap_array_access,
);
criterion_main!(dispatch_benches);
