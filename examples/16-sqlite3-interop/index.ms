let ffi := import "@std/ffi";
let io := import "@std/io";
let core := import "musi:core";
let CString := core.CString;
let CPtr := core.CPtr;

@foreign(abi := .c, symbol := "sqlite3_open")
let sqlite3Open (filename : CString, db : CPtr) : Int32;
@foreign(abi := .c, symbol := "sqlite3_close")
let sqlite3Close (db : CPtr) : Int32;
@foreign(abi := .c, symbol := "sqlite3_exec")
let sqlite3Exec (
  db : CPtr,
  sql : CString,
  callback : CPtr,
  arg : CPtr,
  err : CPtr
) : Int32;

export let createTableSql : CString :=
  "create table if not exists note(id integer primary key, title text);";

export let nullPointer () := ffi.ptr.raw[Unit](ffi.ptr.null[Unit]());

export let demo () :=
  (sqlite3Open, sqlite3Exec, sqlite3Close, createTableSql, nullPointer);
  (
    demo();
    io.writeLn("sqlite interop: declarations and SQL string loaded")
  );
