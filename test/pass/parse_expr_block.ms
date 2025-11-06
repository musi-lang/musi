const a := do { 1; 2; 3 };
const b := do { const x := 5; x + 1 };
const c := do unsafe { ptr_deref(p); };
