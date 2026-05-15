let io := import "@std/io";
let core := import "musi:core";
let CString := core.CString;
let CUInt := core.Nat32;

@foreign(abi := .c, symbol := "glCreateShader")
let glCreateShader (shaderType : CUInt) : CUInt;
@foreign(abi := .c, symbol := "glCompileShader")
let glCompileShader (shader : CUInt) : Unit;
@foreign(abi := .c, symbol := "glCreateProgram")
let glCreateProgram () : CUInt;
@foreign(abi := .c, symbol := "glLinkProgram")
let glLinkProgram (program : CUInt) : Unit;

export let vertexShader : CUInt := 35633n32;

export let fragmentShader : CUInt := 35632n32;

export let vertexSource : CString := "#version 330 core\nvoid main() {}";

export let fragmentSource : CString :=
    "#version 330 core\nout vec4 color; void main() { color = vec4(1.0); }";

export let demo () :=
    (
        glCreateShader,
        glCompileShader,
        glCreateProgram,
        glLinkProgram,
        vertexShader,
        fragmentShader,
        vertexSource,
        fragmentSource,
    );

demo();
io.writeLn("shader pipeline: shader/program declarations loaded");
