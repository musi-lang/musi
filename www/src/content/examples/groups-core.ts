import type { ExampleGroup } from "./types";

export const coreExampleGroups: readonly ExampleGroup[] = [
	{
		id: "option-fallback",
		title: "Fallback value from an optional result",
		caption:
			"One value may be present or missing. Musi does this with the real stdlib <code>Option</code> family.",
		note: "Think of a spare house key: if the usual key exists, use it; otherwise use the backup. Musi expresses that choice with explicit constructors and <code>case</code>.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `int configured = 8080;
int hasConfigured = 1;

int port = hasConfigured ? configured : 3000;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 42,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `std::optional<int> configured = 8080;
int port = configured.value_or(3000);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 51,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `int? configured = 8080;
int port = configured ?? 3000;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 60,
				},
			},
			go: {
				language: "go",
				sourceText: `var configured *int
value := 8080
configured = &value

port := 3000
if configured != nil {
    port = *configured
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 69,
				},
			},
			java: {
				language: "java",
				sourceText: `Optional<Integer> configured = Optional.of(8080);
int port = configured.orElse(3000);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 79,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const configured = 8080;
const port = configured ?? 3000;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 88,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let configured := Option.some[Int](8080);
let port := Option.unwrap_or[Int](configured, 3000);`,
				evidence: {
					path: "packages/std/option/index.ms",
					line: 6,
				},
			},
			python: {
				language: "python",
				sourceText: `configured: int | None = 8080
port = configured if configured is not None else 3000`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 106,
				},
			},
			rust: {
				language: "rust",
				sourceText: `let configured = Some(8080);
let port = configured.unwrap_or(3000);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 115,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const configured: number | null = 8080;
const port = configured ?? 3000;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 124,
				},
			},
		},
	},
	{
		id: "double-function",
		title: "Small function that doubles a number",
		caption:
			"Same small task across multiple languages. Musi keeps it as an expression-oriented <code>let</code> binding.",
		note: "Like doubling a recipe: same operation, different kitchens. Musi keeps the function as a normal binding so it reads like other definitions.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `int twice(int x) {
    return x + x;
}

int answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 130,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `auto twice = [](int x) {
    return x + x;
};

int answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 140,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `Func<int, int> twice = x => x + x;

var answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 150,
				},
			},
			go: {
				language: "go",
				sourceText: `func twice(x int) int {
    return x + x
}

answer := twice(21)`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 159,
				},
			},
			java: {
				language: "java",
				sourceText: `IntUnaryOperator twice = x -> x + x;

int answer = twice.applyAsInt(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 171,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const twice = function (x) {
    return x + x;
};

const answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 180,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let twice (x : Int) : Int := x + x;

let answer := twice(21);`,
				evidence: {
					path: "docs/what/language/syntax.md",
					line: 5,
				},
			},
			python: {
				language: "python",
				sourceText: `def twice(x: int) -> int:
    return x + x

answer = twice(21)`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 193,
				},
			},
			rust: {
				language: "rust",
				sourceText: `fn twice(x: i32) -> i32 {
    x + x
}

let answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 202,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const twice = function (x: number): number {
    return x + x;
};

const answer = twice(21);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 211,
				},
			},
		},
	},
	{
		id: "import-stdlib",
		title: "Import a standard library module",
		caption:
			"Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.",
		note: "Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `#include <stdio.h>

char value[8];
snprintf(value, sizeof(value), "%d", 1);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 218,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `#include <vector>

std::vector<int> value{1};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 228,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `using System;

var value = DateTime.UnixEpoch;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 237,
				},
			},
			go: {
				language: "go",
				sourceText: `import (
    "fmt"
)

value := fmt.Sprintf("%d", 1)`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 246,
				},
			},
			java: {
				language: "java",
				sourceText: `import java.util.Optional;

Optional<Integer> value = Optional.of(1);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 254,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `import { readFileSync } from "node:fs";

const value = readFileSync("musi.json", "utf8");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 262,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let Option := import "@std/option";

let value := Option.some(1);`,
				evidence: {
					path: "packages/std/option/index.ms",
					line: 2,
				},
			},
			python: {
				language: "python",
				sourceText: `import pathlib

value = pathlib.Path.cwd()`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 279,
				},
			},
			rust: {
				language: "rust",
				sourceText: `use std::path::PathBuf;

let value = PathBuf::from(".");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 287,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `import { readFileSync } from "node:fs";

const value = readFileSync("musi.json", "utf8");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 295,
				},
			},
		},
	},
	{
		id: "testing-entry",
		title: "Single package test entry",
		caption:
			"A small test entry should read like ordinary code. Musi uses <code>export let test ()</code> inside <code>*.test.ms</code> files.",
		note: "Think smoke detector checks: small, repeatable, and run regularly. Musi discovers these by file name and runs them with <code>musi test</code>.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `#include <assert.h>

void sums_values(void) {
    assert(1 + 2 == 3);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 297,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `#include <cassert>

void sums_values() {
    assert(1 + 2 == 3);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 307,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `[Fact]
public void SumsValues() {
    Assert.Equal(3, 1 + 2);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 317,
				},
			},
			go: {
				language: "go",
				sourceText: `func TestSumsValues(t *testing.T) {
    got := 1 + 2
    if got != 3 {
        t.Fatalf("want 3, got %d", got)
    }
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 326,
				},
			},
			java: {
				language: "java",
				sourceText: `@Test
void sumsValues() {
    Assertions.assertEquals(3, 1 + 2);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 338,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `test("adds values", () => {
    expect(1 + 2).toBe(3);
});`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 347,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let Testing := import "@std/testing";
export let test () := Testing.it("adds values", Testing.to_be(1 + 2, 3));`,
				evidence: {
					path: "packages/std/option/index.test.ms",
					line: 4,
				},
			},
			python: {
				language: "python",
				sourceText: `def test_sums_values() -> None:
    assert 1 + 2 == 3`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 362,
				},
			},
			rust: {
				language: "rust",
				sourceText: `#[test]
fn sums_values() {
    assert_eq!(1 + 2, 3);
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 370,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `test("adds values", () => {
    expect(1 + 2).toBe(3);
});`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 378,
				},
			},
		},
	},
	{
		id: "data-named-record",
		title: "Named record data definition",
		caption:
			"Use named fields directly in a <code>data</code> definition, then construct values from that shape.",
		note: "Like filling out a passport form: named boxes with known defaults. This form keeps field intent explicit in the type itself.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `typedef struct {
    const char* name;
    int age;
} User;

User user = { .name = "Ada", .age = 0 };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 386,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `struct User {
    std::string name;
    int age = 0;
};

User user{.name = "Ada"};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 396,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `public record User(string Name, int Age = 0);

var user = new User("Ada");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 406,
				},
			},
			go: {
				language: "go",
				sourceText: `type User struct {
    Name string
    Age  int
}

user := User{Name: "Ada"}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 414,
				},
			},
			java: {
				language: "java",
				sourceText: `record User(String name, int age) {}

User user = new User("Ada", 0);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 424,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const user = {
    name: "Ada",
    age: 0,
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 432,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let User := data {
  name : String;
  age : Int := 0;
};

let user : User := { name := "Ada" };`,
				evidence: {
					path: "grammar/Musi.g4",
					line: 157,
				},
			},
			python: {
				language: "python",
				sourceText: `from dataclasses import dataclass

@dataclass
class User:
    name: str
    age: int = 0

user = User("Ada")`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 450,
				},
			},
			rust: {
				language: "rust",
				sourceText: `struct User {
    name: String,
    age: i32,
}

let user = User {
    name: "Ada".to_string(),
    age: 0,
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 462,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `type User = {
    name: string;
    age: number;
};

const user: User = { name: "Ada", age: 0 };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 476,
				},
			},
		},
	},
	{
		id: "record-array-spread",
		title: "Record and array spread updates",
		caption:
			"Build structured values, spread them, and update selected fields in one expression flow.",
		note: "Like copying a form and editing only one line instead of rewriting everything. Spread/update keeps the unchanged parts intact. Musi also has a nested record-update form inspired by F# and OCaml; the note below covers that separately.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `int xs[] = {1, 2};
int ys[] = {0, 1, 2, 3};

struct Point p = {1, 2};
struct Point q = {p.x, 9};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 489,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `std::vector<int> xs{1, 2};
std::vector<int> ys{0};
ys.insert(ys.end(), xs.begin(), xs.end());
ys.push_back(3);

auto p = std::pair{1, 2};
auto q = std::pair{p.first, 9};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 499,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `var xs = new[] { 1, 2 };
var ys = new[] { 0, ..xs, 3 };

var p = new { x = 1, y = 2 };
var q = p with { y = 9 };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 511,
				},
			},
			go: {
				language: "go",
				sourceText: `xs := []int{1, 2}
ys := append([]int{0}, xs...)
ys = append(ys, 3)

type Point struct{ X, Y int }
p := Point{X: 1, Y: 2}
q := Point{X: p.X, Y: 9}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 521,
				},
			},
			java: {
				language: "java",
				sourceText: `var xs = List.of(1, 2);
var ys = Stream.concat(Stream.of(0), Stream.concat(xs.stream(), Stream.of(3))).toList();

record Point(int x, int y) {}
var p = new Point(1, 2);
var q = new Point(p.x(), 9);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 532,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const xs = [1, 2];
const ys = [0, ...xs, 3];

const p = { x: 1, y: 2 };
const q = { ...p, y: 9 };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 543,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let xs := [1, 2];
let ys := [0, ...xs, 3];

let p := { x := 1, y := 2 };
let q := { ...p, y := 9 };`,
				evidence: {
					path: "crates/music_sema/src/tests.rs",
					line: 1192,
				},
			},
			python: {
				language: "python",
				sourceText: `xs = [1, 2]
ys = [0, *xs, 3]

p = {"x": 1, "y": 2}
q = {**p, "y": 9}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 560,
				},
			},
			rust: {
				language: "rust",
				sourceText: `let xs = vec![1, 2];
let mut ys = vec![0];
ys.extend(xs.iter().copied());
ys.push(3);

struct Point { x: i32, y: i32 }
let p = Point { x: 1, y: 2 };
let q = Point { y: 9, ..p };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 569,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const xs = [1, 2];
const ys = [0, ...xs, 3];

const p = { x: 1, y: 2 };
const q = { ...p, y: 9 };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 581,
				},
			},
		},
	},
] as const;
