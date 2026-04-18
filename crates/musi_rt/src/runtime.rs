use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_foundation::extend_import_map;
use musi_native::NativeHost;
use musi_vm::{Program, Vm};
use music_session::SessionOptions;

use crate::api::RuntimeOptions;
use crate::error::RuntimeResult;
use crate::output::{RuntimeOutputSink, RuntimeOutputSinkCell};
use crate::runtime_handlers::register_runtime_handlers;

mod compile;
mod execution;
mod inspection;
mod registration;
mod session_errors;
mod syntax_eval;
mod testing;

type ModuleTextMap = HashMap<Box<str>, String>;
type ProgramMap = HashMap<Box<str>, Program>;
type RuntimeStoreCell = Rc<RefCell<RuntimeStore>>;
type RuntimeProgramResult = RuntimeResult<Program>;

#[derive(Default)]
struct RuntimeStore {
    module_texts: ModuleTextMap,
    programs: ProgramMap,
    session_options: SessionOptions,
}

pub struct Runtime {
    store: RuntimeStoreCell,
    host: NativeHost,
    vm: Option<Vm>,
    options: RuntimeOptions,
    output: RuntimeOutputSinkCell,
    root_spec: Option<Box<str>>,
}

impl Runtime {
    #[must_use]
    pub fn new(mut host: NativeHost, options: RuntimeOptions) -> Self {
        let mut session_options = options.session.clone();
        extend_import_map(&mut session_options.import_map);
        let store = Rc::new(RefCell::new(RuntimeStore {
            session_options,
            ..RuntimeStore::default()
        }));
        let output = RuntimeOutputSink::shared(options.output);
        let nested_host = host.downgrade();
        syntax_eval::register_syntax_handlers(
            &mut host,
            Rc::clone(&store),
            &nested_host,
            &options.vm,
        );
        register_runtime_handlers(&mut host, &output);
        Self {
            store,
            host,
            vm: None,
            options,
            output,
            root_spec: None,
        }
    }
}
