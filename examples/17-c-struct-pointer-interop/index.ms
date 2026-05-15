let io := import "@std/io";
let core := import "musi:core";
let CPtr := core.CPtr;
let CInt := core.Int32;

@foreign(abi := .c, symbol := "demo_point_new")
let demoPointNew (x : CInt, y : CInt) : CPtr;
@foreign(abi := .c, symbol := "demo_point_x")
let demoPointX (point : CPtr) : CInt;
@foreign(abi := .c, symbol := "demo_point_free")
let demoPointFree (point : CPtr) : Unit;

export let pointShape := (10, 20);

export let demo () := (demoPointNew, demoPointX, demoPointFree, pointShape);

demo();
io.writeLn("C interop: struct pointer API declarations loaded");
