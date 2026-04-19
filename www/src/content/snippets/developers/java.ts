import type { ContentSnippet } from "../types";

export const javaDeveloperSnippets = [
	{
		id: "java-values-methods-expressions",
		language: "musi",
		sourceText: `let receiptTotal (latteCents : Int, tipCents : Int) : Int := latteCents + tipCents;

let answer := receiptTotal(450, 120);
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

let selected := render(port := 8080, secure := 1 = 1);
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
    let deliveryFee := 45;
    basePrice + deliveryFee
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
		sourceText: `let checkIns := mut 0;
checkIns := checkIns + 1;

let nextCheckIns := checkIns + 1;
nextCheckIns;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-fresh-value",
		language: "musi",
		sourceText: `let baseFare := 1200;
let totalFare := baseFare + 45;
totalFare;`,
		evidence: {
			path: "docs/what/language/developers/java/variables-mutation.md",
			line: 1,
		},
	},
	{
		id: "java-records-classes-objects",
		language: "musi",
		sourceText: `let Shipment := data {
  host : String;
  dock : Int;
  refrigerated : Bool;
};

let local := {
  host := "Rotterdam",
  dock := 12,
  refrigerated := 0 = 1
};

let express := { ...local, host := "Hamburg", refrigerated := 0 = 0 };
express.dock;`,
		evidence: {
			path: "docs/what/language/developers/java/records-classes-objects.md",
			line: 1,
		},
	},
	{
		id: "java-collections-streams-pipelines",
		language: "musi",
		sourceText: `let iter := import "@std/iter";

let attendees := ["Ava", "Mina", "Noah", "Iris"];
let visible := attendees
  |> iter.append[String]("Luca")
  |> iter.collect[String]();
visible;`,
		evidence: {
			path: "docs/what/language/developers/java/collections-streams-pipelines.md",
			line: 1,
		},
	},
	{
		id: "java-null-option",
		language: "musi",
		sourceText: `let option := import "@std/option";

let lookupLocker (badgeId : String) : option.Option[Int] :=
  match badgeId (
  | "A12" => option.someOf[Int](204)
  | _ => option.noneOf[Int]()
  );

let locker := lookupLocker("B07")
  |> option.unwrapOr[Int](808);
locker;`,
		evidence: {
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-result-value",
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
			path: "docs/what/language/developers/java/null-option-result.md",
			line: 1,
		},
	},
	{
		id: "java-exceptions-results",
		language: "musi",
		sourceText: `let result := import "@std/result";

let parseSeatCount (text : String) : result.Result[Int, String] :=
  match text (
  | "42" => result.ok[Int, String](42)
  | _ => result.err[Int, String]("parse error")
  );

let seats := parseSeatCount("abc")
  |> result.unwrapOr[Int, String](0);
seats;`,
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-effect-boundary",
		language: "musi",
		sourceText: `let io := import "@std/io";

let name := io.promptTrimmed("name> ");
io.writeLine(name);`,
		evidence: {
			path: "docs/what/language/developers/java/exceptions-effects.md",
			line: 1,
		},
	},
	{
		id: "java-sealed-types-patterns",
		language: "musi",
		sourceText: `let DeliveryState := data {
  | Waiting
  | InTransit(stop : Int)
  | Delivered(code : Int)
};

let state : DeliveryState := .InTransit(stop := 3);
match state (
| .InTransit(stop) => stop
| .Waiting => 0
| .Delivered(code) => code
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

let dock := identity[Int](8080);
dock;`,
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

let Bus := data {
  | Bus
};

let busVehicle := instance Vehicle[Bus] {
  let wheels(self : Bus) : Int := 6;
};`,
		evidence: {
			path: "docs/what/language/developers/java/generics-interfaces-laws.md",
			line: 1,
		},
	},
	{
		id: "java-methods-receiver-calls",
		language: "musi",
		sourceText: `let (subtotal : Int).withShipping (shipping : Int) : Int := subtotal + shipping;

let publicTotal := 8080.withShipping(1);
publicTotal;`,
		evidence: {
			path: "docs/what/language/developers/java/methods-and-receiver-calls.md",
			line: 1,
		},
	},
	{
		id: "java-module-export",
		language: "musi",
		sourceText: "export let defaultDock () : Int := 8080;",
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-module-import",
		language: "musi",
		sourceText: `let Shipping := import "./shipping";

let dock := Shipping.defaultDock();
dock;`,
		evidence: {
			path: "docs/what/language/developers/java/packages-modules.md",
			line: 1,
		},
	},
	{
		id: "java-testing-tooling",
		language: "musi",
		sourceText: `let testing := import "@std/testing";

testing.describe("shipping defaults");
testing.it("default dock", testing.equal[Int](8080, 8080));
testing.endDescribe();`,
		evidence: {
			path: "docs/what/language/developers/java/testing-tooling.md",
			line: 1,
		},
	},
	{
		id: "java-native-unsafe-ffi",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

foreign "c" let puts (message : ffi.CString) : ffi.CInt;

export let announce (message : ffi.CString) : ffi.CInt :=
  unsafe { puts(message); };`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
	{
		id: "java-ffi-pointer",
		language: "musi",
		sourceText: `let ffi := import "@std/ffi";

let pointer := ffi.ptr.null[Int]();
let samePointer := unsafe { ffi.ptr.offset[Int](pointer, 0); };
ffi.ptr.isNull[Int](samePointer);`,
		evidence: {
			path: "docs/what/language/developers/java/native-unsafe-ffi.md",
			line: 1,
		},
	},
] satisfies readonly ContentSnippet[];
