import type { ContentSnippet } from "../types";

export const luaDeveloperSnippets = [
	{
		id: "lua-values-locals-expressions",
		language: "musi",
		sourceText: `let orderTotal (basePrice : Int, rushFee : Int) : Int := basePrice + rushFee;

let charge := orderTotal(2400, 150);
charge;`,
		evidence: {
			path: "docs/what/language/developers/lua/overview.md",
			line: 1,
		},
	},
	{
		id: "lua-named-calls",
		language: "musi",
		sourceText: `let renderBadge (port : Int, secure : Bool) : Int := port;

let selectedPort := renderBadge(port := 8080, secure := 1 = 1);
selectedPort;`,
		evidence: {
			path: "docs/what/language/developers/lua/values-locals-expressions.md",
			line: 1,
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
			line: 1,
		},
	},
	{
		id: "lua-block-expression",
		language: "musi",
		sourceText: `let receiptTotal () : Int :=
  (
    let subtotal := 1200;
    let handling := 45;
    subtotal + handling
  );

receiptTotal();`,
		evidence: {
			path: "docs/what/language/developers/lua/blocks-branching-repetition.md",
			line: 1,
		},
	},
	{
		id: "lua-recursive-control-flow",
		language: "musi",
		sourceText: `let rec countSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => countSeats(remaining - 1, seats + 4)
  );

countSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/lua/blocks-branching-repetition.md",
			line: 1,
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
			line: 1,
		},
	},
	{
		id: "lua-arrays-pipelines",
		language: "musi",
		sourceText: `let iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> iter.append[Int](9000)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/lua/arrays-sequences-maps-pipelines.md",
			line: 1,
		},
	},
	{
		id: "lua-map-option",
		language: "musi",
		sourceText: `let option := import "@std/option";

let lookupPort (name : String) : option.Option[Int] :=
  match name (
  | "admin" => option.someOf[Int](9000)
  | "web" => option.someOf[Int](8080)
  | _ => option.noneOf[Int]()
  );

lookupPort("web")
  |> option.unwrapOr[Int](3000);`,
		evidence: {
			path: "docs/what/language/developers/lua/arrays-sequences-maps-pipelines.md",
			line: 1,
		},
	},
	{
		id: "lua-nil-option",
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
			path: "docs/what/language/developers/lua/nil-option-result.md",
			line: 1,
		},
	},
	{
		id: "lua-result-value",
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
			path: "docs/what/language/developers/lua/nil-option-result.md",
			line: 1,
		},
	},
	{
		id: "lua-errors-results",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsePort (text : String) : result.Result[Int, String] :=
  match text (
  | "8080" => result.ok[Int, String](8080)
  | _ => result.err[Int, String]("parse error")
  );

let port := parsePort("abc")
  |> result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/lua/errors-pcall-effects.md",
			line: 1,
		},
	},
	{
		id: "lua-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/lua/errors-pcall-effects.md",
			line: 1,
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
			line: 1,
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
			line: 1,
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
			line: 1,
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
			line: 1,
		},
	},
	{
		id: "lua-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/lua/modules-imports-exports.md",
			line: 1,
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
			line: 1,
		},
	},
	{
		id: "lua-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

testing.describe("ports");
testing.it("default port", testing.equal[Int](8080, 8080));
testing.endDescribe();`,
		evidence: {
			path: "docs/what/language/developers/lua/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "lua-ffi-boundary",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/lua/embedding-c-api-ffi.md",
			line: 1,
		},
	},
	{
		id: "lua-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/lua/embedding-c-api-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
