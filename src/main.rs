use std::path::Path;
use std::process;

use clap::{Arg, ArgAction, Command};

use musi::VERSION;
use musi::engine::VM;
use musi::errors::MusiResult;

fn main() {
    let matches = Command::new("musi")
        .version(VERSION)
        .about("Musi Runtime")
        .arg(
            Arg::new("debug")
                .long("debug")
                .global(true)
                .action(ArgAction::SetTrue)
                .help("Enable debug output"),
        )
        .subcommand_required(true)
        .subcommand(
            Command::new("run")
                .about("Execute bytecode file")
                .arg(
                    Arg::new("file")
                        .required(true)
                        .help("Bytecode file to execute"),
                )
                .arg(Arg::new("procedure").help("Procedure to execute (default: main)")),
        )
        .subcommand(
            Command::new("disasm")
                .about("Disassemble bytecode file")
                .arg(
                    Arg::new("file")
                        .required(true)
                        .help("Bytecode file to disassemble"),
                ),
        )
        .subcommand(
            Command::new("check").about("Check bytecode file").arg(
                Arg::new("file")
                    .required(true)
                    .help("Bytecode file to check"),
            ),
        )
        .subcommand(
            Command::new("info")
                .about("Show module information")
                .arg(Arg::new("file").required(true).help("Bytecode file")),
        )
        .subcommand(
            Command::new("list")
                .about("List procedures in module")
                .arg(Arg::new("file").required(true).help("Bytecode file")),
        )
        .subcommand(
            Command::new("consts")
                .about("List constants in module")
                .arg(Arg::new("file").required(true).help("Bytecode file")),
        )
        .get_matches();

    if let Err(why) = run(&matches) {
        eprintln!("error: {why}");
        process::exit(1);
    }
}

fn get_module_name(file: &str) -> &str {
    Path::new(file)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
}

fn run(matches: &clap::ArgMatches) -> MusiResult<()> {
    let debug = matches.get_flag("debug");

    match matches.subcommand() {
        Some(("run", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            let procedure = sub
                .get_one::<String>("procedure")
                .map(|s| s.as_str())
                .unwrap_or("main");
            let mut vm = VM::with_debug(debug);
            vm.load_module(file)?;
            let result = vm.exec_proc(get_module_name(file), procedure)?;
            if !debug {
                println!("{result}");
            }
            Ok(())
        }
        Some(("disasm", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            let mut vm = VM::new();
            vm.load_module(file)?;
            let disasm = vm.disass_module(get_module_name(file))?;
            println!("{disasm}");
            Ok(())
        }
        Some(("validate", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            if debug {
                println!("validating {file}...");
            }
            let mut vm = VM::new();
            vm.load_module(file)?;
            vm.check_all_modules()?;
            println!("{file} is valid");
            Ok(())
        }
        Some(("info", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            let mut vm = VM::new();
            vm.load_module(file)?;
            let info = vm.get_module_info(get_module_name(file))?;
            println!("{info}");
            Ok(())
        }
        Some(("list", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            let mut vm = VM::new();
            vm.load_module(file)?;
            let module_name = get_module_name(file);
            let procs = vm.list_procs(module_name)?;
            println!("Procedures in {module_name}:");
            for proc in procs {
                println!("  {proc}");
            }
            Ok(())
        }
        Some(("consts", sub)) => {
            let file = sub.get_one::<String>("file").unwrap();
            let mut vm = VM::new();
            vm.load_module(file)?;
            let consts = vm.get_const_info(get_module_name(file))?;
            println!("{consts}");
            Ok(())
        }
        _ => unreachable!(),
    }
}
