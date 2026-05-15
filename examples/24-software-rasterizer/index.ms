let ffi := import "@std/ffi";
let fmt := import "@std/fmt";
let fs := import "@std/fs";
let io := import "@std/io";
let path := import "@std/path";
let text := import "@std/text";
let CString := ffi.CString;
let CPtr := ffi.CPtr;
let CUInt := ffi.CUInt;

export let monkeyObjPath :=
  path.fromString("examples/24-software-rasterizer/assets/monkey.obj");

export hidden let GlfwStart := data {
  | GlfwUnavailable
  | GlfwReady(started : Int32)
};

export hidden let Camera := data {
  | Camera(x : Int, y : Int, zoom : Int)
};

@link("glfw")
@foreign(abi := .c, symbol := "glfwInit")
let glfwInit () : Int32;
@link("glfw")
@foreign(abi := .c, symbol := "glfwCreateWindow")
let glfwCreateWindow (
  width : Int32,
  height : Int32,
  title : CString,
  monitor : CPtr,
  share : CPtr
) : CPtr;
@link("glfw")
@foreign(abi := .c, symbol := "glfwMakeContextCurrent")
let glfwMakeContextCurrent (window : CPtr) : Unit;
@link("glfw")
@foreign(abi := .c, symbol := "glfwPollEvents")
let glfwPollEvents () : Unit;
@link("glfw")
@foreign(abi := .c, symbol := "glfwSwapBuffers")
let glfwSwapBuffers (window : CPtr) : Unit;
@link("glfw")
@foreign(abi := .c, symbol := "glfwWindowShouldClose")
let glfwWindowShouldClose (window : CPtr) : Int32;
@link("glfw")
@foreign(abi := .c, symbol := "glfwGetKey")
let glfwGetKey (window : CPtr, key : Int32) : Int32;
@link("glfw")
@foreign(abi := .c, symbol := "glfwDestroyWindow")
let glfwDestroyWindow (window : CPtr) : Unit;
@link("glfw")
@foreign(abi := .c, symbol := "glfwTerminate")
let glfwTerminate () : Unit;

@link("opengl")
@foreign(abi := .c, symbol := "glClear")
let glClear (mask : CUInt) : Unit;

export let colorBufferBit : CUInt := 16384n32;

export let keyEscape : Int32 := 256;

export let keyA : Int32 := 65;

export let keyD : Int32 := 68;

export let keyS : Int32 := 83;

export let keyW : Int32 := 87;

export let keyMinus : Int32 := 45;

export let keyEqual : Int32 := 61;

export let keyPress : Int32 := 1;

export let startGlfw () :=
  unsafe (
  let started := glfwInit();
  if started = 0 then .GlfwUnavailable else .GlfwReady(started := started)
);

export let keyDown (window : CPtr, key : Int32) :=
  unsafe (glfwGetKey(window, key)) = keyPress;

export let nextCamera (window : CPtr, camera : Camera) :=
  match camera (
  | .Camera(x := x, y := y, zoom := zoom) => .Camera(
    x := if keyDown(window, keyA) then x - 1
    else if keyDown(window, keyD) then x + 1
    else x,
    y := if keyDown(window, keyS) then y - 1
    else if keyDown(window, keyW) then y + 1
    else y,
    zoom := if keyDown(window, keyMinus) then zoom - 1
    else if keyDown(window, keyEqual) then zoom + 1 else zoom,
  )
  );

let recur renderLoop (window : CPtr, camera : Camera) :=
  unsafe (
    if glfwWindowShouldClose(window) = 0 then
    (
      glClear(colorBufferBit);
      glfwPollEvents();
      glfwSwapBuffers(window);
      renderLoop(window, nextCamera(window, camera))
    ) else ()
);

export let loadMonkeyObj () :=
  if fs.exists(monkeyObjPath) then
  (
    let source := fs.readText(monkeyObjPath);
    io.writeLn(
      text.concat("loaded monkey.obj bytes: ", fmt.int(source.length()))
    );
    source
  )
else
  (
    io.writeLn(
      "missing monkey.obj; run examples/24-software-rasterizer/fetch-assets.sh"
    );
    ""
  );

export let demo () :=
  unsafe (
  loadMonkeyObj();
  io.writeLn("opening GLFW/OpenGL rasterizer playground");
  io.writeLn("controls: W/A/S/D move camera, -/= zoom, close window to finish");
  let .GlfwReady(started := started) := startGlfw()
  else
    (
      io.writeLn("GLFW init failed");
      -1
    );
  defer glfwTerminate();
  let window := glfwCreateWindow(
    800,
    600,
    "Musi monkey rasterizer playground",
    ffi.ptr.raw[Unit](ffi.ptr.null[Unit]()),
    ffi.ptr.raw[Unit](ffi.ptr.null[Unit]()),
  );
  defer glfwDestroyWindow(window);
  glfwMakeContextCurrent(window);
  renderLoop(window, .Camera(x := 0, y := 0, zoom := 0));
  io.writeLn("closed rasterizer playground");
  started
);

demo();
