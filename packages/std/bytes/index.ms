let build := import "./_build.ms";
let compare := import "./_compare.ms";

export let empty := build.empty;
export let single := build.single;
export let copy := build.copy;
export let concat := build.concat;
export let append := build.append;
export let prepend := build.prepend;

export let equals := compare.equals;
export let notEquals := compare.notEquals;
export let isEmpty := compare.isEmpty;
export let nonEmpty := compare.nonEmpty;
