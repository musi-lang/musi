let ffi := import "@std/ffi";
let io := import "@std/io";
let CString := ffi.CString;
let CPtr := ffi.CPtr;

export hidden let GlfwStart := data {
  | GlfwUnavailable
  | GlfwReady(started : Int32)
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
@foreign(abi := .c, symbol := "glfwDestroyWindow")
let glfwDestroyWindow (window : CPtr) : Unit;
@link("glfw")
@foreign(abi := .c, symbol := "glfwTerminate")
let glfwTerminate () : Unit;

export let startGlfw () :=
  unsafe (
  let started := glfwInit();
  if started = 0 then .GlfwUnavailable else .GlfwReady(started := started)
);

let recur windowLoop (window : CPtr) :=
  unsafe (
    if glfwWindowShouldClose(window) = 0 then
    (
      glfwPollEvents();
      glfwSwapBuffers(window);
      windowLoop(window)
    ) else ()
);

export let demo () :=
  unsafe (
  let .GlfwReady(started := started) := startGlfw()
  else
    (
      io.writeLn("GLFW init failed");
      -1
    );
  defer glfwTerminate();
  let window := glfwCreateWindow(
    480,
    320,
    "Musi GLFW example",
    ffi.ptr.raw[Unit](ffi.ptr.null[Unit]()),
    ffi.ptr.raw[Unit](ffi.ptr.null[Unit]()),
  );
  defer glfwDestroyWindow(window);
  glfwMakeContextCurrent(window);
  windowLoop(window);
  started
);
demo();
