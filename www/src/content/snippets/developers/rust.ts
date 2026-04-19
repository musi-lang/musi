import type { ContentSnippet } from "../types";

export const rustDeveloperSnippets = [
	{
		id: "rust-values-functions",
		language: "musi",
		sourceText: `let total (orderTotal : Int, serviceFee : Int) : Int := orderTotal + serviceFee;

let invoiceTotal := total(1200, 45);
invoiceTotal;`,
		evidence: {
			path: "docs/what/language/developers/rust/values-functions.md",
			line: 1,
		},
	},
	{
		id: "rust-named-arguments",
		language: "musi",
		sourceText: `let render (port : Int, secure : Bool) : Int := port;

let selected := render(port := 8080, secure := 0 = 0);
selected;`,
		evidence: {
			path: "docs/what/language/developers/rust/values-functions.md",
			line: 1,
		},
	},
	{
		id: "rust-mutation-counter",
		language: "musi",
		sourceText: `let queuedJobs := mut 0;
queuedJobs := queuedJobs + 1;
queuedJobs;`,
		evidence: {
			path: "docs/what/language/developers/rust/mutation.md",
			line: 1,
		},
	},
	{
		id: "rust-fresh-value",
		language: "musi",
		sourceText: `let base := 1200;
let total := base + 45;
total;`,
		evidence: {
			path: "docs/what/language/developers/rust/mutation.md",
			line: 1,
		},
	},
	{
		id: "rust-struct-record",
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
			path: "docs/what/language/developers/rust/records-structs.md",
			line: 1,
		},
	},
	{
		id: "rust-enum-data-match",
		language: "musi",
		sourceText: `let Port := data {
  | Configured(port : Int)
  | Default
};

let selected : Port := .Configured(port := 8080);
match selected (
| .Configured(port) => port
| .Default => 3000
);`,
		evidence: {
			path: "docs/what/language/developers/rust/enums-data.md",
			line: 1,
		},
	},
	{
		id: "rust-trait-class-law",
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
			path: "docs/what/language/developers/rust/traits-classes-laws.md",
			line: 1,
		},
	},
	{
		id: "rust-generic-function",
		language: "musi",
		sourceText: `let identity[T] (input : T) : T := input;

let port := identity[Int](8080);
port;`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-generic-data",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let boxed := .Box1(value := 8080);
match boxed (
| .Box1\\(value) => value
);`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-type-constructor-class",
		language: "musi",
		sourceText: `let Box1[T] := data {
  | Box1(value : T)
};

let Keeps[F : Type -> Type] := class {
  let keep(value : F[Int]) : F[Int];
};

let boxKeeps := instance Keeps[Box1] {
  let keep(value : Box1[Int]) : Box1[Int] := value;
};`,
		evidence: {
			path: "docs/what/language/developers/rust/generics.md",
			line: 1,
		},
	},
	{
		id: "rust-result-data",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parsed : result.Result[Int, String] := result.ok[Int, String](8080);
parsed
  |> result.unwrapOr[Int, String](3000);`,
		evidence: {
			path: "docs/what/language/developers/rust/results-effects.md",
			line: 1,
		},
	},
	{
		id: "rust-effect-request",
		language: "musi",
		sourceText: `let io := import "@std/io";

let line := io.readTrimmedLine();
line;`,
		evidence: {
			path: "docs/what/language/developers/rust/results-effects.md",
			line: 1,
		},
	},
	{
		id: "rust-module-export",
		language: "musi",
		sourceText: "export let defaultPort () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/rust/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "rust-module-import",
		language: "musi",
		sourceText: `let Ports := import "./ports";

let port := Ports.defaultPort();
port;`,
		evidence: {
			path: "docs/what/language/developers/rust/modules-packages.md",
			line: 1,
		},
	},
	{
		id: "rust-unsafe-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let get_counter () : CPtr;

let counter := unsafe {
  ffi.ptr.cast[Int](get_counter());
};`,
		evidence: {
			path: "docs/what/language/developers/rust/unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "rust-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

let defaultPort () : Int := 8080;

export let test () :=
  (
    testing.describe("ports");
    testing.it("default port is http alt", testing.toBe(defaultPort(), 8080));
    testing.endDescribe()
  );`,
		evidence: {
			path: "docs/what/language/developers/rust/testing-tooling.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
