import type { ContentSnippet } from "../types";

export const javascriptTypeScriptDeveloperSnippets = [
	{
		id: "js-ts-values-functions",
		language: "musi",
		sourceText: `let total (base : Int, fee : Int) : Int := base + fee;

let answer := total(1200, 45);
answer;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 1,
		},
	},
	{
		id: "js-ts-named-calls",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 1,
		},
	},
	{
		id: "js-ts-mutable-state",
		language: "musi",
		sourceText: `let visits := mut 0;
visits := visits + 1;
visits;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/state.md",
			line: 1,
		},
	},
	{
		id: "js-ts-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/state.md",
			line: 1,
		},
	},
	{
		id: "js-ts-object-record",
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

let secure := { ...local, secure := 0 = 0 };
secure.port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/objects-records.md",
			line: 1,
		},
	},
	{
		id: "js-ts-arrays-pipelines",
		language: "musi",
		sourceText: `let Iter := import "@std/iter";

let ports := [3000, 8080, 9000];
let visible := ports
  |> Iter.map[Int, Int](\\(port : Int) : Int => port + 1);
visible;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/arrays-pipelines.md",
			line: 1,
		},
	},
	{
		id: "js-ts-null-option",
		language: "musi",
		sourceText: `let Option := import "@std/option";

let findPort (name : String) : Option.Option[Int] := Option.some[Int](8080);

let port := findPort("local")
  |> Option.unwrapOr[Int](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 1,
		},
	},
	{
		id: "js-ts-result-data",
		language: "musi",
		sourceText: `let Result := import "@std/result";

let parsePort (text : String) : Result.Result[Int, String] := Result.ok[Int, String](8080);

let port := parsePort("8080")
  |> Result.unwrapOr[Int, String](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 1,
		},
	},
	{
		id: "js-ts-union-variant",
		language: "musi",
		sourceText: `let LoadState := data {
  | Loading
  | Loaded(value : Int)
  | Failed(message : String)
};

let state : LoadState := .Loaded(value := 8080);
match state (
| .Loaded\\(value) => value
| .Loading => 3000
| .Failed(message) => 3000
);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/unions-variants.md",
			line: 1,
		},
	},
	{
		id: "js-ts-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/generics.md",
			line: 1,
		},
	},
	{
		id: "js-ts-generic-data",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let boxed := .Box1(value := 8080);
match boxed (
| .Box1\\(value) => value
);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/generics.md",
			line: 1,
		},
	},
	{
		id: "js-ts-promise-effect",
		language: "musi",
		sourceText: `let Io := import "@std/io";

let name := Io.promptTrimmed("name> ");
Io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/promises-effects.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "js-ts-class-instance",
		language: "musi",
		sourceText: `let Vehicle[T] := class {
  let wheels(self : T) : Int;
};

let Car := data {
  | Car
};

let carVehicle := instance Vehicle[Car] {
  let wheels(self : Car) : Int := 4;
};`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/classes-behavior.md",
			line: 1,
		},
	},
	{
		id: "js-ts-testing-tooling",
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
			path: "docs/what/language/developers/javascript-typescript/testing-tooling.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
