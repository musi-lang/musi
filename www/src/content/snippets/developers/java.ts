import type { ContentSnippet } from "../types";

export const javaDeveloperSnippets = [
	{
		id: "java-values-methods-expressions",
		language: "musi",
		sourceText: `let total (basePrice : Int, fee : Int) : Int := basePrice + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/java/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "java-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/java/values-methods-expressions.md",
			line: 1,
		},
	},
	{
		id: "java-block-expression",
		language: "musi",
		sourceText: `let invoiceTotal () : Int :=
  (
    let basePrice := 1200;
    let fee := 45;
    basePrice + fee
  );

invoiceTotal();`,
		evidence: {
			path: "docs/what/language/developers/java/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "java-recursive-control-flow",
		language: "musi",
		sourceText: `let rec totalSeats (remaining : Int, seats : Int) : Int :=
  match remaining (
  | 0 => seats
  | _ => totalSeats(remaining - 1, seats + 4)
  );

totalSeats(3, 0);`,
		evidence: {
			path: "docs/what/language/developers/java/blocks-control-flow.md",
			line: 1,
		},
	},
	{
		id: "java-variables-mutation",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;

let nextVisits := visits + 1;
nextVisits;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-fresh-value",
		language: "musi",
		sourceText: `let basePrice := 1200;
let total := basePrice + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-records-classes-objects",
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
			path: "docs/what/language/developers/java/records-classes-objects.md",
			line: 1,
		},
	},
	{
		id: "java-collections-streams-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080];
let visible := ports
  |> Iter.append[Int](9000)
  |> Iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/java/collections-streams-pipelines.md",
			line: 1,
		},
	},
	{
		id: "java-null-option",
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
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-result-value",
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
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-exceptions-results",
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
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-effect-boundary",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-sealed-types-patterns",
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
			path: "docs/what/language/developers/java/sealed-types-patterns.md",
			line: 1,
		},
	},
	{
		id: "java-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "java-interface-class-law",
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
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "java-methods-receiver-calls",
		language: "musi",
		sourceText: `let (port : Int).withOffset (offset : Int) : Int := port + offset;

let publicPort := 8080.withOffset(1);
publicPort;`,
		evidence: {
			path: "docs/what/language/developers/java/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "java-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-testing-tooling",
		language: "musi",
		sourceText: `let Testing := import "@std/testing";

let suite := Testing.suite("ports", [
  Testing.test("default port", \\() => Testing.expectEqual[Int](8080, 8080))
]);

Testing.run(suite);`,
		evidence: {
			path: "docs/what/language/developers/java/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "java-native-unsafe-ffi",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

foreign "c" let puts (message : Ffi.CString) : Ffi.CInt;

export let announce (message : Ffi.CString) : Ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "java-ffi-pointer",
		language: "musi",
		sourceText: `let Ffi := import "@std/ffi";

let pointer := Ffi.ptr.null[Int]();
let samePointer := unsafe { Ffi.ptr.offset[Int](pointer, 0); };
Ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
