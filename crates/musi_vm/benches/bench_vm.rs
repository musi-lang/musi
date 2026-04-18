use std::hint::black_box;
use std::time::Duration;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};

use musi_foundation::register_modules;
use musi_vm::{Program, Value, Vm, VmOptions};
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

fn lookup_answer(vm: &mut Vm) -> Value {
    vm.lookup_export("answer")
        .expect("answer export should resolve")
}

fn int_grid() -> Value {
    let ty = TypeId::from_raw(0);
    let first = Value::sequence(ty, [Value::Int(1), Value::Int(2)]);
    let second = Value::sequence(ty, [Value::Int(3), Value::Int(4)]);
    Value::sequence(ty, [first, second])
}

fn bench_answer_with_int_arg(
    c: &mut Criterion,
    name: &str,
    source: &str,
    arg: i64,
    failure: &'static str,
) {
    let program = compile_program(source);
    let mut vm = initialized_vm(&program, VmOptions);
    let answer = lookup_answer(&mut vm);
    let args = [Value::Int(arg)];

    _ = c.bench_function(name, |b| {
        b.iter(|| {
            let result = vm
                .call_value(black_box(answer.clone()), black_box(&args))
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
        export let answer () : Int := base + offset;
        ",
    );

    _ = c.bench_function("bench_vm_init_small_module", |b| {
        b.iter(|| {
            let mut vm = Vm::with_rejecting_host(black_box(program.clone()), VmOptions);
            vm.initialize().expect("vm init should succeed");
            black_box(vm.executed_instructions())
        });
    });
}

fn bench_vm_call_scalar_recursive_sum(c: &mut Criterion) {
    bench_answer_with_int_arg(
        c,
        "bench_vm_call_scalar_recursive_sum",
        r"
        let rec sum (n : Int, acc : Int) : Int :=
          match n (
          | 0 => acc
          | _ => sum(n - 1, acc + n)
        );
        export let answer (n : Int) : Int := sum(n, 0);
        ",
        200,
        "scalar call should succeed",
    );
}

fn bench_vm_closure_capture(c: &mut Criterion) {
    bench_answer_with_int_arg(
        c,
        "bench_vm_closure_capture",
        r"
        let apply (f : Int -> Int, x : Int) : Int := f(x);
        export let answer (x : Int) : Int := (
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
        export let answer (grid : mut [2][2]Int) : Int := (
          grid.[0, 1] := 42;
          grid.[1, 0] := grid.[0, 1] + 1;
          grid.[0, 1] + grid.[1, 0]
        );
        ",
    );
    let mut vm = initialized_vm(&program, VmOptions);
    let answer = lookup_answer(&mut vm);

    _ = c.bench_function("bench_vm_sequence_index_mutation", |b| {
        b.iter_batched(
            || [int_grid()],
            |args| {
                let result = vm
                    .call_value(black_box(answer.clone()), black_box(&args))
                    .expect("sequence mutation should succeed");
                black_box(result)
            },
            BatchSize::SmallInput,
        );
    });
}

fn bench_vm_data_match_option(c: &mut Criterion) {
    bench_answer_with_int_arg(
        c,
        "bench_vm_data_match_option",
        r"
        let MaybeInt := data {
          | Some(Int)
          | None
        };
        export let answer (n : Int) : Int := (
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
        export let answer () : Int :=
          handle request Console.readLine() using Console {
            value => value + 1;
            readLine(k) => resume 41;
          };
        ",
    );
    let mut vm = initialized_vm(&program, VmOptions);
    let answer = lookup_answer(&mut vm);

    _ = c.bench_function("bench_vm_effect_resume", |b| {
        b.iter(|| {
            let result = vm
                .call_value(black_box(answer.clone()), black_box(&[]))
                .expect("effect resume should succeed");
            black_box(result)
        });
    });
}

fn bench_vm_gc_stress_sequence_return(c: &mut Criterion) {
    let program = compile_program(
        r"
        export let answer () : [8]Int := [0, 1, 2, 3, 4, 5, 6, 7];
        ",
    );

    let mut group = c.benchmark_group("bench_vm_gc_stress_sequence_return");
    _ = group.sample_size(20);
    _ = group.measurement_time(Duration::from_secs(4));
    _ = group.bench_function("fresh_vm_gc_stress", |b| {
        b.iter_batched(
            || initialized_vm(&program, VmOptions.with_gc_stress(true)),
            |mut vm| {
                let result = vm
                    .call_export("answer", &[])
                    .expect("sequence return should succeed");
                let stats = vm.collect_garbage();
                black_box((result, stats.after_bytes))
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
    bench_vm_gc_stress_sequence_return,
);
criterion_main!(benches);
