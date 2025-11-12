use crate::types::{Bytecode, ProcTable, ValueList};

pub unsafe fn link_modules(
    consts_vecs: Vec<ValueList>,
    procs_vecs: Vec<ProcTable>,
    code_vecs: Vec<*mut Vec<u8>>,
) -> Bytecode {
    let (mut all_consts, mut all_procs, mut all_code) = (Vec::new(), Vec::new(), Vec::new());
    let mut code_offset = 0u32;

    for consts in consts_vecs {
        all_consts.extend((*consts).iter());
    }
    for procs in procs_vecs {
        let proc_offset = code_offset;
        for proc in (*procs).iter() {
            let mut proc_copy = *proc;
            proc_copy.code_offset = proc_offset;
            all_procs.push(proc_copy);
        }
    }
    for code in code_vecs {
        let code_len = (**code).len() as u32;
        all_code.extend_from_slice(&**code);
        code_offset += code_len;
    }

    let (consts_ptr, procs_ptr, code_ptr) = (
        Box::into_raw(Box::new(all_consts)),
        Box::into_raw(Box::new(all_procs)),
        Box::into_raw(Box::new(all_code)),
    );

    Bytecode {
        consts: consts_ptr,
        procs: procs_ptr,
        code: code_ptr,
    }
}
