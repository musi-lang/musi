let Build := import "./_build.ms";
let Compare := import "./_compare.ms";

export let empty := Build.empty;
export let single := Build.single;
export let copy := Build.copy;
export let concat := Build.concat;
export let append := Build.append;
export let prepend := Build.prepend;

export let equals := Compare.equals;
export let notEquals := Compare.notEquals;
export let isEmpty := Compare.isEmpty;
export let nonEmpty := Compare.nonEmpty;
