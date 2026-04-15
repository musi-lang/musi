import type { ContentSnippet } from "../types";

export const cCppDeveloperSnippets = [
	{
		id: "c-cpp-values-functions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-structs-classes-records",
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
			path: "docs/what/language/developers/c-cpp/structs-classes-records.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-arrays-pointers-slices",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/arrays-pointers-slices.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-null-option",
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
			path: "docs/what/language/developers/c-cpp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-result-value",
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
			path: "docs/what/language/developers/c-cpp/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-errors-results",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] :=
  match text (
  | "8080" => Result.ok[Int, String](8080)
  | _ => Result.err[Int, String]("parse error")
  );

let port := parsePort("abc")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-enums-variants-patterns",
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
			path: "docs/what/language/developers/c-cpp/enums-variants-patterns.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-concept-class-law",
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
			path: "docs/what/language/developers/c-cpp/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/c-cpp/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let suite := Testing.suite("ports", [
  Testing.test("default port", \\() => Testing.expectEqual[Int](8080, 8080))
]);

Testing.run(suite);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
	{
		id: "c-cpp-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/c-cpp/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
