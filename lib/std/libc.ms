--! Safe C library module.

let ascii := import "@std/ascii";
let cmp := import "@std/cmp";
let ffi := import "@std/ffi";
let CInt := ffi.CInt;
let CString := ffi.CString;
let Int32 := ffi.CInt;
let Nat := ffi.CSize;
let Ordering := cmp.Ordering;

@foreign(abi := .c, symbol := "strcmp")
let C__strcmp (left : CString, right : CString) : Int32;
@foreign(abi := .c, symbol := "strerror")
let C__strerror (code : CInt) : CString;
@foreign(abi := .c, symbol := "strlen")
let C__strlen (value : CString) : Nat;

--- C `strcmp`.
export let strcmp (left : CString, right : CString) :=
  unsafe (C__strcmp(left, right));
--- C `strerror`.
export let strerror (code : CInt) := unsafe (C__strerror(code));
--- C `strlen`.
export let strlen (value : CString) := unsafe (C__strlen(value));

--- C string byte length.
export let length (value : CString) := strlen(value);

--- C string comparison as ordering.
export let compare (left : CString, right : CString) :=
  (
    let value := strcmp(left, right);
    if value < 0 then cmp.less()
    else if value > 0 then cmp.greater() else cmp.equal();
  );

--- Returns true when C strings compare equal.
export let equals (left : CString, right : CString) :=
  cmp.isEqual(compare(left, right));

--- C runtime error message text for an error code.
export let errorMessage (code : CInt) := strerror(code);

--- Safe ASCII replacement for locale-sensitive C `isalnum`.
export let isAlnum (value : Int) := ascii.isAlphanumeric(value);
--- Safe ASCII replacement for locale-sensitive C `isalpha`.
export let isAlpha (value : Int) := ascii.isAlpha(value);
--- Safe ASCII replacement for locale-sensitive C `isdigit`.
export let isDigit (value : Int) := ascii.isDigit(value);
--- Safe ASCII replacement for locale-sensitive C `isxdigit`.
export let isXDigit (value : Int) := ascii.isHexDigit(value);
--- Safe ASCII replacement for locale-sensitive C `isspace`.
export let isSpace (value : Int) := ascii.isWhitespace(value);
--- Safe ASCII replacement for locale-sensitive C `isprint`.
export let isPrint (value : Int) := ascii.isPrintable(value);
--- Safe ASCII replacement for locale-sensitive C `iscntrl`.
export let isCntrl (value : Int) := ascii.isControl(value);
--- Safe ASCII replacement for locale-sensitive C `tolower`.
export let toLower (value : Int) := ascii.toLower(value);
--- Safe ASCII replacement for locale-sensitive C `toupper`.
export let toUpper (value : Int) := ascii.toUpper(value);
