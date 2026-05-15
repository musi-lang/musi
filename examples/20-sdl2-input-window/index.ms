let io := import "@std/io";
let core := import "musi:core";
let CString := core.CString;
let CPtr := core.CPtr;

@foreign(abi := .c, symbol := "SDL_Init")
let sdlInit (flags : Nat32) : Int32;
@foreign(abi := .c, symbol := "SDL_CreateWindow")
let sdlCreateWindow (
  title : CString,
  x : Int32,
  y : Int32,
  w : Int32,
  h : Int32,
  flags : Nat32
) : CPtr;
@foreign(abi := .c, symbol := "SDL_PollEvent")
let sdlPollEvent (event : CPtr) : Int32;
@foreign(abi := .c, symbol := "SDL_Quit")
let sdlQuit () : Unit;

export let sdlInitVideo := 32n32;

export let demo () :=
  (sdlInit, sdlCreateWindow, sdlPollEvent, sdlQuit, sdlInitVideo);

demo();
io.writeLn("SDL2: input/window declarations loaded");
