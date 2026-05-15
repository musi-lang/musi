let io := import "@std/io";
let core := import "musi:core";
let CUInt := core.Nat32;

@foreign(abi := .c, symbol := "glClearColor")
let glClearColor (r : Float, g : Float, b : Float, a : Float) : Unit;
@foreign(abi := .c, symbol := "glClear")
let glClear (mask : CUInt) : Unit;
@foreign(abi := .c, symbol := "glDrawArrays")
let glDrawArrays (mode : CUInt, first : Int32, count : Int32) : Unit;

export let triangles : CUInt := 4n32;

export let colorBufferBit : CUInt := 16384n32;

export let triangleVertices := [0.0, 0.8, -0.8, -0.8, 0.8, -0.8];

export let demo () :=
  (
    glClearColor,
    glClear,
    glDrawArrays,
    triangles,
    colorBufferBit,
    triangleVertices,
  );

demo();
io.writeLn("OpenGL: triangle draw declarations and vertices loaded");
