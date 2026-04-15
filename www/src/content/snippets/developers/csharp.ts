import type { ContentSnippet } from "../types";

export const csharpDeveloperSnippets = [
	{
		id: "csharp-values-methods-expressions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/csharp/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "csharp-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/csharp/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "csharp-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/csharp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "csharp-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/csharp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "csharp-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/csharp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "csharp-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/csharp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "csharp-records-classes-objects",
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

let publicEndpoint := { ...local, host := "api.example.com", secure := 0 = 0 };
publicEndpoint.port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/records-classes-objects.md",
			line: 1,
		},
	},
	{
		id: "csharp-collections-linq-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/csharp/collections-linq-pipelines.md",
			line: 1,
		},
	},
	{
		id: "csharp-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | _ => Option.none[Int]()
  );

let port := lookupPort("web")
  |> Option.unwrapOr[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-result-value",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("invalid port")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-exceptions-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("format error")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "csharp-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "csharp-unions-pattern-matching",
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
			path: "docs/what/language/developers/csharp/unions-pattern-matching.md",
			line: 1,
		},
	},
	{
		id: "csharp-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "csharp-interface-class-law",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
  law atLeastFourWheels(vehicle : T) := vehicle.wheels() >= 4;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/csharp/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "csharp-extension-methods-and-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/csharp/extension-methods-and-calls.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-import",
		language: "musi",
		sourceText: `let Defaults := import "./defaults";

let port := Defaults.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    Testing.describe("ports");
    Testing.it("default port is http alt", Testing.toBe(defaultPort(), 8080));
    Testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/csharp/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "csharp-unsafe-interop-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
	{
		id: "csharp-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
