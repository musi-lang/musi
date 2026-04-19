import type { ContentSnippet } from "../types";

export const cpp17DeveloperSnippets = [
	{
		id: "cpp17-values-functions",
		language: "musi",
		sourceText: `let orderTotal (basePrice : Int, rushFee : Int) : Int := basePrice + rushFee;

let charge := orderTotal(2400, 150);
charge;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "cpp17-named-calls",
		language: "musi",
		sourceText: `let renderBadge (port : Int, secure : Bool) : Int := port;

let selectedPort := renderBadge(port := 8080, secure := 1 = 1);
selectedPort;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/values-functions-expressions.md",
			line: 1,
		},
	},
	{
		id: "cpp17-block-expression",
		language: "musi",
		sourceText: `let receiptTotal () : Int :=
  (
    let subtotal := 1200;
    let handling := 45;
    subtotal + handling
  );

receiptTotal();`,
		evidence: {
			path: "docs/what/language/developers/cpp17/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "cpp17-recursive-control-flow",
		language: "musi",
		sourceText: `let rec countSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => countSeats(remaining - 1, seats + 4)
  );

countSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/cpp17/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "cpp17-variables-mutation",
		language: "musi",
		sourceText: `let queueDepth := mut 0;
queueDepth := queueDepth + 1;

let nextDepth := queueDepth + 1;
nextDepth;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "cpp17-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "cpp17-structs-classes-records",
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
			path: "docs/what/language/developers/cpp17/structs-classes-records.md",
			line: 1,
		},
	},
	{
		id: "cpp17-arrays-pointers-slices",
		language: "musi",
		sourceText: `let iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> iter.append[Int](9000)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/arrays-pointers-slices.md",
			line: 1,
		},
	},
	{
		id: "cpp17-null-option",
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
			path: "docs/what/language/developers/cpp17/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "cpp17-result-value",
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
			path: "docs/what/language/developers/cpp17/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "cpp17-errors-results",
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
			path: "docs/what/language/developers/cpp17/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "cpp17-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/cpp17/errors-results-effects.md",
			line: 1,
		},
	},
	{
		id: "cpp17-enums-variants-patterns",
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
			path: "docs/what/language/developers/cpp17/enums-variants-patterns.md",
			line: 1,
		},
	},
	{
		id: "cpp17-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "cpp17-concept-class-law",
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
			path: "docs/what/language/developers/cpp17/templates-concepts-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "cpp17-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "cpp17-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/cpp17/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "cpp17-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/cpp17/headers-modules-packages.md",
			line: 1,
		},
	},
	{
		id: "cpp17-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

testing.describe("ports");
testing.it("default port", testing.equal[Int](8080, 8080));
testing.endDescribe();`,
		evidence: {
			path: "docs/what/language/developers/cpp17/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "cpp17-unsafe-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/cpp17/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
	{
		id: "cpp17-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/cpp17/unsafe-ffi-native-boundaries.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
