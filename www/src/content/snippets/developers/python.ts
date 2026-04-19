import type { ContentSnippet } from "../types";

export const pythonDeveloperSnippets = [
	{
		id: "python-values-functions",
		language: "musi",
		sourceText: `let total (orderTotal : Int, serviceFee : Int) : Int := orderTotal + serviceFee;

let invoiceTotal := total(1200, 45);
invoiceTotal;`,
		evidence: {
			path: "docs/what/language/developers/python/values-functions.md",
			line: 1,
		},
	},
	{
		id: "python-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/python/values-functions.md",
			line: 1,
		},
	},
	{
		id: "python-blocks-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

let seats := totalSeats(3, 0);
seats;`,
		evidence: {
			path: "docs/what/language/developers/python/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "python-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal :=
  (
    let base := 1200;
    let fee := 45;
    base + fee
  );

invoiceTotal;`,
		evidence: {
			path: "docs/what/language/developers/python/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "python-names-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/python/names-mutation.md",
			line: 1,
		},
	},
	{
		id: "python-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/python/names-mutation.md",
			line: 1,
		},
	},
	{
		id: "python-records-objects-dicts",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let local := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let public := { ...local, host := "api.example.com", secure := 0 = 0 };
public.host;`,
		evidence: {
			path: "docs/what/language/developers/python/records-objects-dicts.md",
			line: 1,
		},
	},
	{
		id: "python-collections-pipelines",
		language: "musi",
		sourceText: `let iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> iter.append[Int](9000)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/python/collections-pipelines.md",
			line: 1,
		},
	},
	{
		id: "python-none-option",
		language: "musi",
		sourceText: `let option := import "@std/option";

let lookupPort (name : String) : option.Option[Int] :=
  match name (
  | "admin" => option.someOf[Int](9000)
  | _ => option.noneOf[Int]()
  );

let port := lookupPort("web")
  |> option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/none-option-result.md",
			line: 1,
		},
	},
	{
		id: "python-result-value",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsePort (text : String) : result.Result[Int, String] :=
  match text (
  | "8080" => result.ok[Int, String](8080)
  | _ => result.err[Int, String]("port must be numeric")
  );

let port := parsePort("abc")
  |> result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/none-option-result.md",
			line: 1,
		},
	},
	{
		id: "python-exceptions-results",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsePort (text : String) : result.Result[Int, String] :=
  match text (
  | "8080" => result.ok[Int, String](8080)
  | _ => result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "python-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/python/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "python-data-variants-patterns",
		language: "musi",
		sourceText: `let TaskState := data {
  | Waiting
  | Running(id : Int)
  | Done(code : Int)
};

let state : TaskState := .Running(id := 42);
match state (
| .Running(id) => id
| .Waiting => 0
| .Done(code) => code
);`,
		evidence: {
			path: "docs/what/language/developers/python/data-variants-patterns.md",
			line: 1,
		},
	},
	{
		id: "python-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/python/types-generics-protocols.md",
			line: 1,
		},
	},
	{
		id: "python-protocol-class",
		language: "musi",
		sourceText: `let Sized[T] := class {
  let size(self : T) : Int;
};

let Crate := data {
  | Crate(items : Int)
};

let crateSized := instance Sized[Crate] {
  let size(self : Crate) : Int :=
    match self (
    | .Crate(items) => items
    );
};`,
		evidence: {
			path: "docs/what/language/developers/python/types-generics-protocols.md",
			line: 1,
		},
	},
	{
		id: "python-module-export",
		language: "musi",
		sourceText: `// ports.ms
export let defaultPort () : Int := 8080;`,
		evidence: {
			path: "docs/what/language/developers/python/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "python-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/python/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "python-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    testing.describe("ports");
    testing.it("default port is http alt", testing.toBe(defaultPort(), 8080));
    testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/python/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "python-native-unsafe-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/python/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "python-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/python/native-unsafe-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
