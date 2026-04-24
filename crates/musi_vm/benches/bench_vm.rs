use std::hint::black_box;
use std::time::{Duration, Instant};

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};

use musi_foundation::register_modules;
use musi_vm::{
    BoundI64Call, BoundInitCall, BoundSeq2x2Call, BoundSeq8Call, Program, Value, Vm, VmOptions,
};
use music_module::ModuleKey;
use music_seam::TypeId;
use music_session::{Session, SessionOptions};

fn compile_program(source: &str) -> Program {
    let mut session = Session::new(SessionOptions::default());
    register_modules(&mut session).expect("foundation modules should install");
    session
        .set_module_text(&ModuleKey::new("main"), source.to_owned())
        .expect("module text should install");
    let output = session
        .compile_entry(&ModuleKey::new("main"))
        .expect("session compile should succeed");
    Program::from_bytes(&output.bytes).expect("program load should succeed")
}

fn initialized_vm(program: &Program, options: VmOptions) -> Vm {
    let mut vm = Vm::with_rejecting_host(program.clone(), options);
    vm.initialize().expect("vm init should succeed");
    vm
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

fn bench_result_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, VmOptions);
    let result = bind_result_i64(&mut vm);

    _ = c.bench_function(name, |b| {
        b.iter(|| {
            let result = vm
                .call_i64_i64(black_box(result), black_box(arg))
                .expect(failure);
            black_box(result)
        });
    });
}

fn bench_vm_init_small_module(c: &mut Criterion) {
    let program = compile_program(
        r"
        let base : Int := 41;
        let offset : Int := 1;
        export let result () : Int := base + offset;
        ",
    );

    _ = c.bench_function("bench_vm_construct_small_module", |b| {
        b.iter(|| {
            let vm = Vm::with_rejecting_host(black_box(program.clone()), VmOptions);
            black_box(vm.executed_instructions())
        });
    });

    _ = c.bench_function("bench_vm_init_small_module", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                for _ in 0..batch {
                    vms.push(Vm::with_rejecting_host(program.clone(), VmOptions));
                }
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

    _ = c.bench_function("bench_vm_init_small_module_pure", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            let mut remaining = iters;
            while remaining > 0 {
                let batch = remaining.min(512);
                let mut vms = Vec::with_capacity(batch as usize);
                for _ in 0..batch {
                    vms.push(Vm::with_rejecting_host(program.clone(), VmOptions));
                }
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
}

fn bench_vm_call_scalar_recursive_sum(c: &mut Criterion) {
    bench_result_with_int_arg(
        c,
        "bench_vm_call_scalar_recursive_sum",
        r"
        let rec sum (n : Int, acc : Int) : Int :=
          match n (
          | 0 => acc
          | _ => sum(n - 1, acc + n)
        );
        export let result (n : Int) : Int := sum(n, 0);
        ",
        200,
        "scalar call should succeed",
    );
}

fn bench_vm_closure_capture(c: &mut Criterion) {
    bench_result_with_int_arg(
        c,
        "bench_vm_closure_capture",
        r"
        let apply (f : Int -> Int, x : Int) : Int := f(x);
        export let result (x : Int) : Int := (
          let base : Int := 41;
          let add_base (y : Int) : Int := y + base;
          apply(add_base, x)
        );
        ",
        1,
        "closure call should succeed",
    );
}

fn bench_vm_sequence_index_mutation(c: &mut Criterion) {
    let program = compile_program(
        r"
        export let result (grid : mut [2][2]Int) : Int := (
          grid.[0, 1] := 42;
          grid.[1, 0] := grid.[0, 1] + 1;
          grid.[0, 1] + grid.[1, 0]
        );
        ",
    );
    let mut vm = initialized_vm(&program, VmOptions);
    let result = bind_result_seq2(&mut vm);
    let Value::Seq(grid) = int_grid(&mut vm) else {
        panic!("grid allocation should return sequence")
    };
    let grid = vm
        .bind_seq2x2_i64_arg(grid)
        .expect("grid should bind to seq2x2 arg");

    _ = c.bench_function("bench_vm_sequence_index_mutation", |b| {
        b.iter(|| {
            let result = grid
                .call_i64(black_box(result))
                .expect("sequence mutation should succeed");
            black_box(result)
        });
    });
}

fn bench_vm_data_match_option(c: &mut Criterion) {
    bench_result_with_int_arg(
        c,
        "bench_vm_data_match_option",
        r"
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
        ",
        41,
        "data match should succeed",
    );
}

fn bench_vm_effect_resume(c: &mut Criterion) {
    let program = compile_program(
        r"
        export let Console := effect {
          let readLine () : Int;
        };
        let consoleAnswer := answer Console {
          value => value + 1;
          readLine(k) => resume 41;
        };
        export let result () : Int :=
          handle ask Console.readLine() answer consoleAnswer;
        ",
    );
    let mut vm = initialized_vm(&program, VmOptions);
    let result = bind_result_init(&mut vm);

    _ = c.bench_function("bench_vm_effect_resume", |b| {
        b.iter(|| {
            let result = vm
                .call_init0_i64(black_box(result))
                .expect("effect resume should succeed");
            black_box(result)
        });
    });
}

fn bench_vm_sequence_return_gc(c: &mut Criterion) {
    let program = compile_program(
        r"
        export let result () : [8]Int := [0, 1, 2, 3, 4, 5, 6, 7];
        ",
    );

    let mut group = c.benchmark_group("bench_vm_sequence_return_gc");
    _ = group.sample_size(20);
    _ = group.measurement_time(Duration::from_secs(4));
    let mut vm = initialized_vm(&program, VmOptions);
    let result = bind_result_seq8(&mut vm);
    _ = group.bench_function("sequence_return_alloc", |b| {
        b.iter(|| {
            let result = vm
                .call_seq8_i64(black_box(result))
                .expect("sequence return should succeed");
            black_box((result, vm.heap_allocated_bytes()))
        });
    });
    _ = group.bench_function("sequence_return_call_export_alloc", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions),
            |mut vm| {
                let result = vm
                    .call_export("result", &[])
                    .expect("sequence return should succeed");
                black_box((result, vm.heap_allocated_bytes()))
            },
            BatchSize::SmallInput,
        );
    });
    _ = group.bench_function("sequence_return_collect", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions),
            |mut vm| {
                let result = vm
                    .call_export("result", &[])
                    .expect("sequence return should succeed");
                let stats = vm.collect_garbage();
                black_box((result, stats.after_bytes))
            },
            BatchSize::SmallInput,
        );
    });
    _ = group.bench_function("sequence_return_gc_stress", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions.with_gc_stress(true)),
            |mut vm| {
                let result = vm
                    .call_export("result", &[])
                    .expect("sequence return should succeed");
                black_box((result, vm.heap_allocated_bytes()))
            },
            BatchSize::SmallInput,
        );
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
