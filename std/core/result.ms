#[lang := "result"]
export let Result := forall 'T, 'E -> Ok of 'T + Err of 'E;
