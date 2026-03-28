import "@std/bytes/concat" as _;
import "@std/bytes/equals" as _;

export let _joined := concat([1, 2], [3, 4]);
export let _same := equals([1, 2], [1, 2]);
