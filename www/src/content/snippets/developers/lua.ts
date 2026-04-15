import type { ContentSnippet } from "../types";

export const luaDeveloperSnippets = [
	{
		id: "lua-values-locals-expressions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/lua/overview.md",
			line: 27,
		},
	},
	{
		id: "lua-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/lua/values-locals-expressions.md",
			line: 35,
		},
	},
	{
		id: "lua-function-closure",
		language: "musi",
		sourceText: `let makeAdder (step : Int) : Int -> Int :=
  \\(value : Int) => value + step;

let addOne := makeAdder(1);
let nextPort := addOne(8080);
nextPort;`,
		evidence: {
			path: "docs/what/language/developers/lua/functions-closures-named-calls.md",
			line: 28,
		},
	},
	{
		id: "lua-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/lua/blocks-branching-repetition.md",
			line: 25,
		},
	},
	{
		id: "lua-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/lua/blocks-branching-repetition.md",
			line: 41,
		},
	},
	{
		id: "lua-tables-records",
		language: "musi",
		sourceText: `let Endpoint := data {
  host : String;
  port : Int;
  secure : Bool;
};

let localEndpoint := {
  host := "localhost",
  port := 8080,
  secure := 0 = 1
};

let publicEndpoint := { ...localEndpoint, host := "api.example.com", secure := 0 = 0 };
publicEndpoint.port;`,
		evidence: {
			path: "docs/what/language/developers/lua/tables-records-field-updates.md",
			line: 26,
		},
	},
	{
		id: "lua-arrays-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/lua/arrays-sequences-maps-pipelines.md",
			line: 22,
		},
	},
	{
		id: "lua-map-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let lookupPort (name : String) : Option.Option[Int] :=
  match name (
  | "admin" => Option.some[Int](9000)
  | "web" => Option.some[Int](8080)
  | _ => Option.none[Int]()
  );

lookupPort("web")
  |> Option.unwrapOr[Int](3000);`,
		evidence: {
			path: "docs/what/language/developers/lua/arrays-sequences-maps-pipelines.md",
			line: 33,
		},
	},
	{
		id: "lua-nil-option",
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
			path: "docs/what/language/developers/lua/nil-option-result.md",
			line: 25,
		},
	},
	{
		id: "lua-result-value",
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
			path: "docs/what/language/developers/lua/nil-option-result.md",
			line: 40,
		},
	},
	{
		id: "lua-errors-results",
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
			path: "docs/what/language/developers/lua/errors-pcall-effects.md",
			line: 31,
		},
	},
	{
		id: "lua-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/lua/errors-pcall-effects.md",
			line: 43,
		},
	},
	{
		id: "lua-effect-request",
		language: "musi",
		sourceText: `let PortSource := effect {
  let loadPort () : Int;
};

let loadPort () using { PortSource } : Int :=
  request PortSource.loadPort();`,
		evidence: {
			path: "docs/what/language/developers/lua/coroutines-effect-boundaries.md",
			line: 26,
		},
	},
	{
		id: "lua-handler-boundary",
		language: "musi",
		sourceText: `let PortSource := effect {
  let loadPort () : Int;
};

handle PortSource.loadPort() using PortSource {
  value => value;
  loadPort(k) => resume 8080;
};`,
		evidence: {
			path: "docs/what/language/developers/lua/coroutines-effect-boundaries.md",
			line: 30,
		},
	},
	{
		id: "lua-metatable-class-law",
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
			path: "docs/what/language/developers/lua/metatables-classes-instances-laws.md",
			line: 28,
		},
	},
	{
		id: "lua-data-variants",
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
			path: "docs/what/language/developers/lua/pattern-matching-data-variants.md",
			line: 25,
		},
	},
	{
		id: "lua-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/lua/modules-imports-exports.md",
			line: 27,
		},
	},
	{
		id: "lua-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/lua/modules-imports-exports.md",
			line: 38,
		},
	},
	{
		id: "lua-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let suite := Testing.suite("ports", [
  Testing.test("default port", \\() => Testing.expectEqual[Int](8080, 8080))
]);

Testing.run(suite);`,
		evidence: {
			path: "docs/what/language/developers/lua/testing-tooling.md",
			line: 25,
		},
	},
	{
		id: "lua-ffi-boundary",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/lua/embedding-c-api-ffi.md",
			line: 22,
		},
	},
	{
		id: "lua-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/lua/embedding-c-api-ffi.md",
			line: 35,
		},
	},
] satisfies readonly ContentSnippet[];
