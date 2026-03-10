#[lang := "propagate"]
export class Propagate over 'T (
    let extract(value: 'T) : 'T;
    let short_circuit(value: 'T) : 'T
)
