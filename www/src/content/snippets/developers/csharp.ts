import type { ContentSnippet } from "../types";

export const csharpDeveloperSnippets = [
	{
		id: "csharp-values-methods-expressions",
		language: "musi",
		sourceText: `let parkingTotal (baseFee : Int, nightFee : Int) : Int := baseFee + nightFee;

let answer := parkingTotal(1200, 45);
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

let selected := render(port := 8080, secure := 1 = 1);
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
		sourceText: `let queueDepth := mut 0;
queueDepth := queueDepth + 1;

let nextQueueDepth := queueDepth + 1;
nextQueueDepth;`,
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
		sourceText: `let iter := import "@std/iter";

let ticketIds := [3000, 8080];
let visible := ticketIds
  |> iter.append[Int](9000)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/csharp/collections-linq-pipelines.md",
			line: 1,
		},
	},
	{
		id: "csharp-null-option",
		language: "musi",
		sourceText: `let option := import "@std/option";

let lookupDock (name : String) : option.Option[Int] :=
  match name (
  | "admin" => option.someOf[Int](9000)
  | _ => option.noneOf[Int]()
  );

let dock := lookupDock("web")
  |> option.unwrapOr[Int](8080);
dock;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-result-value",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parseWeight (text : String) : result.Result[Int, String] :=
  match text (
  | "450" => result.ok[Int, String](450)
  | _ => result.err[Int, String]("invalid weight")
  );

let weight := parseWeight("abc")
  |> result.unwrapOr[Int, String](0);
weight;`,
		evidence: {
			path: "docs/what/language/developers/csharp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "csharp-exceptions-results",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsePort (text : String) : result.Result[Int, String] :=
  match text (
  | "8080" => result.ok[Int, String](8080)
  | _ => result.err[Int, String]("format error")
  );

let port := parsePort("abc")
  |> result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/csharp/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "csharp-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
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

let dock := identity[Int](8080);
dock;`,
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
		sourceText: `let (dock : Int).withOffset (offset : Int) : Int := dock + offset;

let publicDock := 8080.withOffset(1);
publicDock;`,
		evidence: {
			path: "docs/what/language/developers/csharp/extension-methods-and-calls.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-export",
		language: "musi",
		sourceText: "export let defaultDock () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-module-import",
		language: "musi",
		sourceText: `let Defaults := import "./defaults";

let dock := Defaults.defaultDock();
dock;`,
		evidence: {
			path: "docs/what/language/developers/csharp/namespaces-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "csharp-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

let defaultDock () : Int := 8080;

export let test () :=
  (
    testing.describe("shipping defaults");
    testing.it("default dock is http alt", testing.toBe(defaultDock(), 8080));
    testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/csharp/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "csharp-unsafe-interop-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
	{
		id: "csharp-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/csharp/unsafe-interop-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
