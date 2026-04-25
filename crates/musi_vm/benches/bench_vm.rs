use musi_vm::BoundExportCall;
use std::hint::black_box;
use std::slice::from_ref;
use std::time::{Duration, Instant};

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};

use musi_foundation::register_modules;
use musi_vm::{
    BoundI64Call, BoundInitCall, BoundSeq2x2Call, BoundSeq8Call, MvmMode, Program, Value, Vm,
    VmOptions,
};
use music_module::ModuleKey;
use music_seam::TypeId;
use music_session::{Session, SessionOptions};

fn compile_program_bytes(source: &str) -> Vec<u8> {
    let mut session = Session::new(SessionOptions::default());
    register_modules(&mut session).expect("foundation modules should install");
    session
        .set_module_text(&ModuleKey::new("main"), source.to_owned())
        .expect("module text should install");
    session
        .compile_entry(&ModuleKey::new("main"))
        .expect("session compile should succeed")
        .bytes
}

fn load_program(bytes: &[u8]) -> Program {
    Program::from_bytes(bytes).expect("program load should succeed")
}

fn compile_program(source: &str) -> Program {
    load_program(&compile_program_bytes(source))
}

const fn interpreter_options() -> VmOptions {
    VmOptions.with_mode(MvmMode::Interpreter)
}

const fn debug_interpreter_options() -> VmOptions {
    VmOptions.with_mode(MvmMode::DebugInterpreter)
}

fn initialized_vm(program: &Program, options: VmOptions) -> Vm {
    let mut vm = Vm::with_rejecting_host(program.clone(), options);
    vm.initialize().expect("vm init should succeed");
    vm
}

fn load_initialized_vm(bytes: &[u8], options: VmOptions) -> Vm {
    let program = load_program(bytes);
    initialized_vm(&program, options)
}

fn bind_result_i64(vm: &mut Vm) -> BoundI64Call {
    vm.bind_export_i64_i64("result")
        .expect("result export should bind")
}

fn bind_result_init(vm: &mut Vm) -> BoundInitCall {
    vm.bind_export_init0("result")
        .expect("result export should bind")
}

fn bind_result_seq2(vm: &mut Vm) -> BoundSeq2x2Call {
    vm.bind_export_seq2x2_i64("result")
        .expect("result export should bind")
}

fn bind_result_seq8(vm: &mut Vm) -> BoundSeq8Call {
    vm.bind_export_seq8_i64("result")
        .expect("result export should bind")
}

fn bind_result_export(vm: &mut Vm) -> BoundExportCall {
    vm.bind_export_call("result")
        .expect("result export should bind")
}

fn int_grid(vm: &mut Vm) -> Value {
    let ty = TypeId::from_raw(0);
    let first = vm
        .alloc_pair_sequence(ty, Value::Int(1), Value::Int(2))
        .expect("first row should allocate");
    let second = vm
        .alloc_pair_sequence(ty, Value::Int(3), Value::Int(4))
        .expect("second row should allocate");
    vm.alloc_pair_sequence(ty, first, second)
        .expect("grid should allocate")
}

fn call_result_i64(vm: &mut Vm, arg: i64, failure: &'static str) -> Value {
    vm.call_export("result", &[Value::Int(arg)]).expect(failure)
}

fn call_result_unit(vm: &mut Vm, failure: &'static str) -> Value {
    vm.call_export("result", &[]).expect(failure)
}

fn bench_hot_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_i64(&mut vm);

    _ = c.bench_function(name, |b| {
        b.iter(|| {
            let returned_int = vm
                .call_i64_i64(black_box(bound_call), black_box(arg))
                .expect(failure);
            black_box(returned_int)
        });
    });
}

fn bench_interpreter_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    bench_hot_result_with_int_arg(c, name, source, arg, failure);
}

fn bench_debug_interpreter_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, debug_interpreter_options());

    _ = c.bench_function(name, |b| {
        b.iter(|| {
            let returned_value = call_result_i64(&mut vm, black_box(arg), failure);
            black_box(returned_value)
        });
    });
}

fn bench_bound_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    bench_hot_result_with_int_arg(c, name, source, arg, failure);
}

fn bench_normal_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    bench_hot_result_with_int_arg(c, name, source, arg, failure);
}

fn bench_cold_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    let program_bytes = compile_program_bytes(source);

    _ = c.bench_function(name, |b| {
        b.iter(|| {
            let mut vm = load_initialized_vm(black_box(&program_bytes), VmOptions);
            let returned_value = call_result_i64(&mut vm, black_box(arg), failure);
            black_box((returned_value, vm.executed_instructions()))
        });
    });
}

fn bench_vm_init_small_module(c: &mut Criterion) {
    let source = r"
        let base : Int := 41;
        let offset : Int := 1;
        export let result () : Int := base + offset;
        ";
    let program_bytes = compile_program_bytes(source);
    let program = load_program(&program_bytes);

    _ = c.bench_function("bench_vm_hot_vm_mode_construct_small_vm", |b| {
        b.iter(|| {
            let vm = Vm::with_rejecting_host(black_box(program.clone()), VmOptions);
            black_box(vm.executed_instructions())
        });
    });

    _ = c.bench_function("bench_vm_hot_vm_mode_init_small_module", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                vms.extend((0..batch).map(|_| Vm::with_rejecting_host(program.clone(), VmOptions)));
                let start = Instant::now();
                for vm in &mut vms {
                    vm.initialize().expect("vm init should succeed");
                    _ = black_box(vm.executed_instructions());
                }
                total += start.elapsed();
                remaining -= batch;
            }
            total
        });
    });

    _ = c.bench_function("bench_vm_hot_vm_mode_init_small_module_pure", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                vms.extend((0..batch).map(|_| Vm::with_rejecting_host(program.clone(), VmOptions)));
                let start = Instant::now();
                for vm in &mut vms {
                    vm.initialize().expect("vm init should succeed");
                    _ = black_box(vm.executed_instructions());
                }
                total += start.elapsed();
                remaining -= batch;
            }
            total
        });
    });

    _ = c.bench_function("bench_vm_normal_vm_mode_init_small_module", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                vms.extend((0..batch).map(|_| Vm::with_rejecting_host(program.clone(), VmOptions)));
                let start = Instant::now();
                for vm in &mut vms {
                    vm.initialize().expect("vm init should succeed");
                    _ = black_box(vm.executed_instructions());
                }
                total += start.elapsed();
                remaining -= batch;
            }
            total
        });
    });

    _ = c.bench_function("bench_vm_generic_vm_mode_init_small_module", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                vms.extend((0..batch).map(|_| Vm::with_rejecting_host(program.clone(), VmOptions)));
                let start = Instant::now();
                for vm in &mut vms {
                    vm.initialize().expect("vm init should succeed");
                    _ = black_box(vm.executed_instructions());
                }
                total += start.elapsed();
                remaining -= batch;
            }
            total
        });
    });

    _ = c.bench_function("bench_vm_interpreter_vm_mode_init_small_module", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                vms.extend(
                    (0..batch)
                        .map(|_| Vm::with_rejecting_host(program.clone(), interpreter_options())),
                );
                let start = Instant::now();
                for vm in &mut vms {
                    vm.initialize().expect("vm init should succeed");
                    _ = black_box(vm.executed_instructions());
                }
                total += start.elapsed();
                remaining -= batch;
            }
            total
        });
    });

    _ = c.bench_function("bench_vm_cold_vm_mode_init_small_module", |b| {
        b.iter(|| {
            let mut vm = load_initialized_vm(black_box(&program_bytes), VmOptions);
            let returned_value = call_result_unit(&mut vm, "small module call should succeed");
            black_box((returned_value, vm.executed_instructions()))
        });
    });
}

fn bench_vm_call_scalar_recursive_sum(c: &mut Criterion) {
    let source = r"
        let rec sum (n : Int, acc : Int) : Int :=
          match n (
          | 0 => acc
          | _ => sum(n - 1, acc + n)
        );
        export let result (n : Int) : Int := sum(n, 0);
        ";
    bench_hot_result_with_int_arg(
        c,
        "bench_vm_hot_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
    bench_normal_result_with_int_arg(
        c,
        "bench_vm_normal_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
    bench_bound_result_with_int_arg(
        c,
        "bench_vm_generic_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
    bench_interpreter_result_with_int_arg(
        c,
        "bench_vm_interpreter_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
    bench_debug_interpreter_result_with_int_arg(
        c,
        "bench_vm_debug_interpreter_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
    bench_cold_result_with_int_arg(
        c,
        "bench_vm_cold_vm_mode_scalar_recursive_sum",
        source,
        200,
        "scalar call should succeed",
    );
}

fn bench_vm_closure_capture(c: &mut Criterion) {
    let source = r"
        let apply (f : Int -> Int, x : Int) : Int := f(x);
        export let result (x : Int) : Int := (
          let base : Int := 41;
          let add_base (y : Int) : Int := y + base;
          apply(add_base, x)
        );
        ";
    bench_hot_result_with_int_arg(
        c,
        "bench_vm_hot_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
    bench_normal_result_with_int_arg(
        c,
        "bench_vm_normal_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
    bench_bound_result_with_int_arg(
        c,
        "bench_vm_generic_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
    bench_interpreter_result_with_int_arg(
        c,
        "bench_vm_interpreter_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
    bench_debug_interpreter_result_with_int_arg(
        c,
        "bench_vm_debug_interpreter_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
    bench_cold_result_with_int_arg(
        c,
        "bench_vm_cold_vm_mode_closure_capture",
        source,
        1,
        "closure call should succeed",
    );
}

fn bench_vm_sequence_index_mutation(c: &mut Criterion) {
    let source = r"
        export let result (grid : mut [2][2]Int) : Int := (
          grid.[0, 1] := 42;
          grid.[1, 0] := grid.[0, 1] + 1;
          grid.[0, 1] + grid.[1, 0]
        );
        ";
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_seq2(&mut vm);
    let Value::Seq(grid) = int_grid(&mut vm) else {
        panic!("grid allocation should return sequence")
    };
    let grid = vm
        .bind_seq2x2_i64_arg(grid)
        .expect("grid should bind to seq2x2 arg");

    _ = c.bench_function("bench_vm_hot_vm_mode_sequence_index_mutation", |b| {
        b.iter(|| {
            let returned_int = grid.call_i64(bound_call);
            black_box(returned_int)
        });
    });

    let mut vm = initialized_vm(&program, VmOptions);
    let normal_call = bind_result_seq2(&mut vm);
    let Value::Seq(normal_grid) = int_grid(&mut vm) else {
        panic!("grid allocation should return sequence")
    };
    let normal_grid = vm
        .bind_seq2x2_i64_arg(normal_grid)
        .expect("grid should bind to seq2x2 arg");
    _ = c.bench_function("bench_vm_normal_vm_mode_sequence_index_mutation", |b| {
        b.iter(|| {
            let returned_value = normal_grid.call_i64(normal_call);
            black_box(returned_value)
        });
    });

    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_seq2(&mut vm);
    let Value::Seq(grid) = int_grid(&mut vm) else {
        panic!("grid allocation should return sequence")
    };
    let grid = vm
        .bind_seq2x2_i64_arg(grid)
        .expect("grid should bind to seq2x2 arg");
    _ = c.bench_function("bench_vm_generic_vm_mode_sequence_index_mutation", |b| {
        b.iter(|| {
            let returned_int = grid.call_i64(bound_call);
            black_box(returned_int)
        });
    });

    let mut vm = initialized_vm(&program, interpreter_options());
    let bound_call = bind_result_seq2(&mut vm);
    let Value::Seq(interpreter_grid) = int_grid(&mut vm) else {
        panic!("grid allocation should return sequence")
    };
    let interpreter_grid = vm
        .bind_seq2x2_i64_arg(interpreter_grid)
        .expect("grid should bind to seq2x2 arg");
    _ = c.bench_function(
        "bench_vm_interpreter_vm_mode_sequence_index_mutation",
        |b| {
            b.iter(|| {
                let returned_value = interpreter_grid.call_i64(bound_call);
                black_box(returned_value)
            });
        },
    );

    let mut vm = initialized_vm(&program, debug_interpreter_options());
    let debug_grid = int_grid(&mut vm);
    _ = c.bench_function(
        "bench_vm_debug_interpreter_vm_mode_sequence_index_mutation",
        |b| {
            b.iter(|| {
                let returned_value = vm
                    .call_export("result", from_ref(black_box(&debug_grid)))
                    .expect("sequence mutation should succeed");
                black_box(returned_value)
            });
        },
    );

    let program_bytes = compile_program_bytes(source);
    _ = c.bench_function("bench_vm_cold_vm_mode_sequence_index_mutation", |b| {
        b.iter(|| {
            let mut vm = load_initialized_vm(black_box(&program_bytes), VmOptions);
            let grid = int_grid(&mut vm);
            let returned_value = vm
                .call_export("result", &[black_box(grid)])
                .expect("sequence mutation should succeed");
            black_box((returned_value, vm.executed_instructions()))
        });
    });
}

fn bench_vm_data_match_option(c: &mut Criterion) {
    let source = r"
        let MaybeInt := data {
          | Some(Int)
          | None
        };
        export let result (n : Int) : Int := (
          let selected : MaybeInt := .Some(n);
          match selected (
          | .Some(value) => value + 1
          | .None => 0
          )
        );
        ";
    bench_hot_result_with_int_arg(
        c,
        "bench_vm_hot_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
    bench_normal_result_with_int_arg(
        c,
        "bench_vm_normal_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
    bench_bound_result_with_int_arg(
        c,
        "bench_vm_generic_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
    bench_interpreter_result_with_int_arg(
        c,
        "bench_vm_interpreter_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
    bench_debug_interpreter_result_with_int_arg(
        c,
        "bench_vm_debug_interpreter_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
    bench_cold_result_with_int_arg(
        c,
        "bench_vm_cold_vm_mode_data_match_option",
        source,
        41,
        "data match should succeed",
    );
}

fn bench_vm_effect_resume(c: &mut Criterion) {
    let source = r"
        export let Console := effect {
          let readLine () : Int;
        };
        let consoleAnswer := answer Console {
          value => value + 1;
          readLine(k) => resume 41;
        };
        export let result () : Int :=
          handle ask Console.readLine() answer consoleAnswer;
        ";
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_init(&mut vm);

    _ = c.bench_function("bench_vm_hot_vm_mode_effect_resume_equivalent", |b| {
        b.iter(|| {
            let returned_int = vm
                .call_init0_i64(black_box(bound_call))
                .expect("effect resume should succeed");
            black_box(returned_int)
        });
    });

    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_init(&mut vm);
    _ = c.bench_function("bench_vm_normal_vm_mode_effect_resume_equivalent", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_init0_i64(black_box(bound_call))
                .expect("effect resume should succeed");
            black_box(returned_value)
        });
    });

    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_init(&mut vm);
    _ = c.bench_function("bench_vm_generic_vm_mode_effect_resume_equivalent", |b| {
        b.iter(|| {
            let returned_int = vm
                .call_init0_i64(black_box(bound_call))
                .expect("effect resume should succeed");
            black_box(returned_int)
        });
    });

    let mut vm = initialized_vm(&program, interpreter_options());
    let bound_call = bind_result_init(&mut vm);
    _ = c.bench_function(
        "bench_vm_interpreter_vm_mode_effect_resume_equivalent",
        |b| {
            b.iter(|| {
                let returned_value = vm
                    .call_init0_i64(black_box(bound_call))
                    .expect("effect resume should succeed");
                black_box(returned_value)
            });
        },
    );

    let mut vm = initialized_vm(&program, debug_interpreter_options());
    _ = c.bench_function(
        "bench_vm_debug_interpreter_vm_mode_effect_resume_equivalent",
        |b| {
            b.iter(|| {
                let returned_value = call_result_unit(&mut vm, "effect resume should succeed");
                black_box(returned_value)
            });
        },
    );

    let program_bytes = compile_program_bytes(source);
    _ = c.bench_function("bench_vm_cold_vm_mode_effect_resume_equivalent", |b| {
        b.iter(|| {
            let mut vm = load_initialized_vm(black_box(&program_bytes), VmOptions);
            let returned_value = call_result_unit(&mut vm, "effect resume should succeed");
            black_box((returned_value, vm.executed_instructions()))
        });
    });
}

fn bench_vm_sequence_return_gc(c: &mut Criterion) {
    let source = r"
        export let result () : [8]Int := [0, 1, 2, 3, 4, 5, 6, 7];
        ";
    let program_bytes = compile_program_bytes(source);
    let program = load_program(&program_bytes);

    let mut group = c.benchmark_group("bench_vm_sequence_return_gc");
    _ = group.sample_size(20);
    _ = group.measurement_time(Duration::from_secs(4));
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_seq8(&mut vm);
    _ = group.bench_function("hot_vm_mode_sequence_return_alloc", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_seq8_i64(black_box(bound_call))
                .expect("sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_export(&mut vm);
    _ = group.bench_function("hot_vm_mode_sequence_return_bound_export_alloc", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_bound_export(black_box(&bound_call), &[])
                .expect("sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    _ = group.bench_function("hot_vm_mode_sequence_return_call_export_alloc", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions),
            |mut vm| {
                let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
                black_box((returned_value, vm.heap_allocated_bytes()))
            },
            BatchSize::SmallInput,
        );
    });
    let mut vm = initialized_vm(&program, VmOptions);
    vm.prewarm_export_fast_path("result")
        .expect("sequence return should prewarm");
    vm.clear_external_roots();
    _ = group.bench_function(
        "hot_vm_mode_sequence_return_call_export_alloc_reused",
        |b| {
            b.iter(|| {
                let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
                let allocated = vm.heap_allocated_bytes();
                vm.clear_external_roots();
                black_box((returned_value, allocated))
            });
        },
    );
    _ = group.bench_function("hot_vm_mode_sequence_return_collect", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions),
            |mut vm| {
                let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
                let stats = vm.collect_garbage();
                black_box((returned_value, stats.after_bytes))
            },
            BatchSize::SmallInput,
        );
    });
    let mut vm = initialized_vm(&program, VmOptions);
    vm.prewarm_export_fast_path("result")
        .expect("sequence return should prewarm");
    vm.clear_external_roots();
    _ = group.bench_function("hot_vm_mode_sequence_return_collect_reused", |b| {
        b.iter(|| {
            let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
            let stats = vm.collect_garbage();
            vm.clear_external_roots();
            black_box((returned_value, stats.after_bytes))
        });
    });
    _ = group.bench_function("hot_vm_mode_sequence_return_gc_stress", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions.with_gc_stress(true)),
            |mut vm| {
                let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
                black_box((returned_value, vm.heap_allocated_bytes()))
            },
            BatchSize::SmallInput,
        );
    });
    let mut vm = initialized_vm(&program, VmOptions.with_gc_stress(true));
    vm.prewarm_export_fast_path("result")
        .expect("sequence return should prewarm");
    vm.clear_external_roots();
    _ = group.bench_function("hot_vm_mode_sequence_return_gc_stress_reused", |b| {
        b.iter(|| {
            let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
            let allocated = vm.heap_allocated_bytes();
            vm.clear_external_roots();
            black_box((returned_value, allocated))
        });
    });
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_export(&mut vm);
    _ = group.bench_function("normal_vm_mode_sequence_return_alloc", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_bound_export(black_box(&bound_call), &[])
                .expect("sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    let mut vm = initialized_vm(&program, VmOptions);
    let bound_call = bind_result_seq8(&mut vm);
    _ = group.bench_function("generic_vm_mode_sequence_return_alloc", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_seq8_i64(black_box(bound_call))
                .expect("sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    let mut vm = initialized_vm(&program, interpreter_options());
    let bound_call = bind_result_seq8(&mut vm);
    _ = group.bench_function("interpreter_vm_mode_sequence_return_alloc", |b| {
        b.iter(|| {
            let returned_value = vm
                .call_seq8_i64(black_box(bound_call))
                .expect("sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    _ = group.bench_function("debug_interpreter_vm_mode_sequence_return_alloc", |b| {
        b.iter_batched(
            || initialized_vm(&program, debug_interpreter_options()),
            |mut vm| {
                let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
                black_box((returned_value, vm.heap_allocated_bytes()))
            },
            BatchSize::SmallInput,
        );
    });
    _ = group.bench_function("cold_vm_mode_sequence_return_alloc", |b| {
        b.iter(|| {
            let mut vm = load_initialized_vm(black_box(&program_bytes), VmOptions);
            let returned_value = call_result_unit(&mut vm, "sequence return should succeed");
            black_box((returned_value, vm.heap_allocated_bytes()))
        });
    });
    group.finish();
}

criterion_group!(
    benches,
    bench_vm_init_small_module,
    bench_vm_call_scalar_recursive_sum,
    bench_vm_closure_capture,
    bench_vm_sequence_index_mutation,
    bench_vm_data_match_option,
    bench_vm_effect_resume,
    bench_vm_sequence_return_gc,
);
criterion_main!(benches);
