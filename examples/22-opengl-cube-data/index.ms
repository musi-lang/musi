let io := import "@std/io";
let core := import "musi:core";
let CPtr := core.CPtr;
let CUInt := core.Nat32;
let CInt := core.Int32;

@foreign(abi := .c, symbol := "glGenBuffers")
let glGenBuffers (count : CInt, out : CPtr) : Unit;
@foreign(abi := .c, symbol := "glBindBuffer")
let glBindBuffer (target : CUInt, buffer : CUInt) : Unit;
@foreign(abi := .c, symbol := "glBufferData")
let glBufferData (
  target : CUInt,
  size : Nat,
  bytesPtr : CPtr,
  usage : CUInt
) : Unit;

export let arrayBuffer : CUInt := 34962n32;

export let staticDraw : CUInt := 35044n32;

export let cubeVertices :=
  [-1.0, -1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0, -1.0];

export let demo () :=
  (
    glGenBuffers,
    glBindBuffer,
    glBufferData,
    arrayBuffer,
    staticDraw,
    cubeVertices,
  );

demo();
io.writeLn("OpenGL cube: buffer declarations and vertices loaded");
