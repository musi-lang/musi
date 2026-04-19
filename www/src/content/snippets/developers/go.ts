import type { ContentSnippet } from "../types";

export const goDeveloperSnippets = [
	{
		id: "go-values-functions",
		language: "musi",
		sourceText: `let total (orderTotal : Int, serviceFee : Int) : Int := orderTotal + serviceFee;

let invoiceTotal := total(1200, 45);
invoiceTotal;`,
		evidence: {
			path: "docs/what/language/developers/go/overview.md",
			line: 27,
		},
	},
	{
		id: "go-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/go/values-functions-expressions.md",
			line: 37,
		},
	},
	{
		id: "go-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/go/blocks-branching-loops.md",
			line: 25,
		},
	},
	{
		id: "go-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/go/blocks-branching-loops.md",
			line: 41,
		},
	},
	{
		id: "go-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/go/variables-short-declarations-mutation.md",
			line: 24,
		},
	},
	{
		id: "go-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/go/variables-short-declarations-mutation.md",
			line: 37,
		},
	},
	{
		id: "go-structs-records",
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
			path: "docs/what/language/developers/go/structs-records-field-updates.md",
			line: 30,
		},
	},
	{
		id: "go-slices-pipelines",
		language: "musi",
		sourceText: `let iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> iter.append[Int](9000)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/go/slices-arrays-maps-pipelines.md",
			line: 23,
		},
	},
	{
		id: "go-map-option",
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
			path: "docs/what/language/developers/go/slices-arrays-maps-pipelines.md",
			line: 34,
		},
	},
	{
		id: "go-nil-option",
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
			path: "docs/what/language/developers/go/nil-option-result.md",
			line: 25,
		},
	},
	{
		id: "go-result-value",
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
			path: "docs/what/language/developers/go/nil-option-result.md",
			line: 42,
		},
	},
	{
		id: "go-errors-results",
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
			path: "docs/what/language/developers/go/errors-results-effects.md",
			line: 31,
		},
	},
	{
		id: "go-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/go/errors-results-effects.md",
			line: 45,
		},
	},
	{
		id: "go-effect-request",
		language: "musi",
		sourceText: `let PortSource := effect {
  let loadPort () : Int;
};

let loadPort () using { PortSource } : Int :=
  request PortSource.loadPort();`,
		evidence: {
			path: "docs/what/language/developers/go/goroutines-channels-context-effects.md",
			line: 27,
		},
	},
	{
		id: "go-handler-boundary",
		language: "musi",
		sourceText: `let PortSource := effect {
  let loadPort () : Int;
};

handle PortSource.loadPort() using PortSource {
  value => value;
  loadPort(k) => resume 8080;
};`,
		evidence: {
			path: "docs/what/language/developers/go/goroutines-channels-context-effects.md",
			line: 44,
		},
	},
	{
		id: "go-interface-class-law",
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
			path: "docs/what/language/developers/go/interfaces-classes-laws.md",
			line: 29,
		},
	},
	{
		id: "go-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/go/generics-type-constructors.md",
			line: 25,
		},
	},
	{
		id: "go-generic-data",
		language: "musi",
		sourceText: `let Box[T] := data {
  | Box(value : T)
};

let Keeps[F : Type -> Type] := class {
  let keep(value : F[Int]) : F[Int];
};

let boxKeeps := instance Keeps[Box] {
  let keep(value : Box[Int]) : Box[Int] := value;
};

let box := .Box(value := 8080);
boxKeeps.keep(box);`,
		evidence: {
			path: "docs/what/language/developers/go/generics-type-constructors.md",
			line: 39,
		},
	},
	{
		id: "go-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/go/methods-receiver-calls.md",
			line: 27,
		},
	},
	{
		id: "go-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/go/packages-imports-exports.md",
			line: 25,
		},
	},
	{
		id: "go-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/go/packages-imports-exports.md",
			line: 37,
		},
	},
	{
		id: "go-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

testing.describe("ports");
testing.it("default port", testing.equal[Int](8080, 8080));
testing.endDescribe();`,
		evidence: {
			path: "docs/what/language/developers/go/testing-tooling.md",
			line: 25,
		},
	},
	{
		id: "go-unsafe-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/go/unsafe-cgo-ffi.md",
			line: 28,
		},
	},
	{
		id: "go-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/go/unsafe-cgo-ffi.md",
			line: 41,
		},
	},
] satisfies readonly ContentSnippet[];
