import type { ContentSnippet } from "../types";

export const javascriptTypeScriptDeveloperSnippets = [
	{
		id: "js-ts-values-functions",
		language: "musi",
		sourceText: `let receiptTotal (latteCents : Int, croissantCents : Int) : Int := latteCents + croissantCents;

let answer := receiptTotal(450, 120);
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

let selected := render(port := 8080, secure := 1 = 1);
selected;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/values-functions.md",
			line: 1,
		},
	},
	{
		id: "js-ts-mutable-state",
		language: "musi",
		sourceText: `let pageViews := mut 0;
pageViews := pageViews + 1;
pageViews;`,
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
		sourceText: `let iter := import "@std/iter";

let prices := [19, 29, 45];
let visible := prices
  |> iter.append[Int](60)
  |> iter.collect[Int]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/arrays-pipelines.md",
			line: 1,
		},
	},
	{
		id: "js-ts-null-option",
		language: "musi",
		sourceText: `let option := import "@std/option";

let findPort (name : String) : option.Option[Int] := option.someOf[Int](8080);

let port := findPort("local")
  |> option.unwrapOr[Int](3000);
port;`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/null-result.md",
			line: 1,
		},
	},
	{
		id: "js-ts-result-data",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsePort (text : String) : result.Result[Int, String] := result.ok[Int, String](8080);

let port := parsePort("8080")
  |> result.unwrapOr[Int, String](3000);
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

let dock := identity[Int](8080);
dock;`,
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
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/promises-effects.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-export",
		language: "musi",
		sourceText: "export let defaultDock () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "js-ts-module-import",
		language: "musi",
		sourceText: `let Shipping := import "./shipping";

let dock := Shipping.defaultDock();
dock;`,
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
		sourceText: `let testing := import "@std/testing";

let defaultDock () : Int := 8080;

export let test () :=
  (
    testing.describe("shipping defaults");
    testing.it("default dock is http alt", testing.toBe(defaultDock(), 8080));
    testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/javascript-typescript/testing-tooling.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
