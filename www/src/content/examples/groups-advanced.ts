import type { ExampleGroup } from "./types";

export const advancedExampleGroups: readonly ExampleGroup[] = [
	{
		id: "quote-metaprogramming",
		title: "Code as data template (metaprogramming)",
		caption:
			"Build or transform code structure itself, not only runtime values.",
		note: "When a language lacks first-class quote/splice, the closest equivalent is usually macros, AST builders, or plain code generators.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `#define ADD_TEMPLATE(X, D) ((X) + (D))

int y = ADD_TEMPLATE(x, delta);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 900,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `template <typename Expr>
auto add_template(Expr expr, int delta) {
    return expr + delta;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 909,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `using System.Linq.Expressions;

Expression<Func<int, int>> addTemplate = x => x + delta;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 918,
				},
			},
			go: {
				language: "go",
				sourceText: `code := fmt.Sprintf("func add(x int) int { return x + %d }", delta)
// generated source string`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 927,
				},
			},
			java: {
				language: "java",
				sourceText: `var code = "int add(int x){ return x + " + delta + "; }";
// usually processed by annotation processors or codegen tools`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 936,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const code = \`(x) => x + \${delta}\`;
// template string code generation`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 945,
				},
			},
			musi: {
				language: "musi",
				sourceText: "let add_template := quote (#(x) + #(delta));",
				evidence: {
					path: "crates/music_syntax/src/parser/tests.rs",
					line: 76,
				},
			},
			python: {
				language: "python",
				sourceText: `import ast

expr = ast.BinOp(left=ast.Name(id="x"), op=ast.Add(), right=ast.Constant(delta))`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 963,
				},
			},
			rust: {
				language: "rust",
				sourceText: `use quote::quote;

let ast = quote! { x + #delta };`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 972,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const code = \`(x: number) => x + \${delta}\`;
// or use ts-morph / compiler API for AST-level transforms`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 981,
				},
			},
		},
	},
	{
		id: "effect-handle",
		title: "Define, perform, and handle effects",
		caption:
			"Capture side-effect requests in one place, then resolve them through handlers.",
		note: "At small scale this can look like callback wiring, but at larger scale handlers keep policy at boundaries and reduce plumbing across call chains.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `typedef const char* (*ReadlnFn)(void);

const char* read_with(ReadlnFn readln) {
    return readln();
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 593,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `auto readWith = [](auto readln) {
    return readln();
};

auto value = readWith([] { return "ok"; });`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 602,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `string ReadWith(Func<string> readln) {
    return readln();
}

var value = ReadWith(() => "ok");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 612,
				},
			},
			go: {
				language: "go",
				sourceText: `readWith := func(readln func() string) string {
    return readln()
}

value := readWith(func() string { return "ok" })`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 622,
				},
			},
			java: {
				language: "java",
				sourceText: `Supplier<String> readln = () -> "ok";
String value = readln.get();`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 631,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const readWith = (readln) => readln();
const value = readWith(() => "ok");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 638,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let console := effect {
  let readln () : String;
};

handle perform console.readln() with console of (
| readln(k) => resume "ok"
| value => value
);`,
				evidence: {
					path: "crates/music_sema/src/tests.rs",
					line: 263,
				},
			},
			python: {
				language: "python",
				sourceText: `def read_with(readln):
    return readln()

value = read_with(lambda: "ok")`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 656,
				},
			},
			rust: {
				language: "rust",
				sourceText: `fn read_with(readln: impl Fn() -> String) -> String {
    readln()
}

let value = read_with(|| "ok".to_string());`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 665,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `const readWith = (readln: () => string): string => {
    return readln();
};

const value = readWith(() => "ok");`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 674,
				},
			},
		},
	},
	{
		id: "class-instance",
		title: "Typeclass-style constraints and instances",
		caption:
			"Define shared behavior once, then attach concrete implementations per type.",
		note: "Like one wall-socket standard with different appliance designs behind the plug. Declare one behavior shape, then implement it per type.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `typedef int (*EqInt)(int, int);

int eq_int(int a, int b) {
    return a == b;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 687,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `template <typename T>
struct Eq {
    bool operator()(const T& a, const T& b) const { return a == b; }
};

bool same = Eq<int>{}(1, 1);`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 696,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `public interface IEq<T> {
    bool Equal(T a, T b);
}

public sealed class EqInt : IEq<int> {
    public bool Equal(int a, int b) => a == b;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 706,
				},
			},
			go: {
				language: "go",
				sourceText: `type Eq[T comparable] interface {
    Equal(a T, b T) bool
}

type EqInt struct{}
func (EqInt) Equal(a int, b int) bool { return a == b }`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 718,
				},
			},
			java: {
				language: "java",
				sourceText: `interface Eq<T> {
    boolean equal(T a, T b);
}

Eq<Integer> eqInt = Integer::equals;`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 729,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const eqInt = {
    equal(a, b) {
        return a === b;
    },
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 738,
				},
			},
			musi: {
				language: "musi",
				sourceText: `let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
};

let eq_int := instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := .True;
};`,
				evidence: {
					path: "crates/music_sema/src/tests.rs",
					line: 502,
				},
			},
			python: {
				language: "python",
				sourceText: `from typing import Protocol

class Eq(Protocol):
    def equal(self, a: int, b: int) -> bool: ...

class EqInt:
    def equal(self, a: int, b: int) -> bool:
        return a == b`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 756,
				},
			},
			rust: {
				language: "rust",
				sourceText: `trait EqLike<T> {
    fn equal(a: T, b: T) -> bool;
}

struct EqInt;
impl EqLike<i32> for EqInt {
    fn equal(a: i32, b: i32) -> bool { a == b }
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 768,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `interface Eq<T> {
    equal(a: T, b: T): boolean;
}

const eqInt: Eq<number> = {
    equal(a, b) {
        return a === b;
    },
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 780,
				},
			},
		},
	},
	{
		id: "generic-constraint",
		title: "Generic constraint with explicit capability",
		caption:
			"Constrain polymorphic code to capabilities that must exist at call sites.",
		note: "Like requiring a driving license before renting a car: callers must provide the needed capability. Musi writes that requirement with <code>where T : Eq</code> before the result type annotation.",
		defaultLanguage: "musi",
		variants: {
			c: {
				language: "c",
				sourceText: `int require_eq_int(int x, int (*eq)(int, int)) {
    return eq(x, x) ? x : 0;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 795,
				},
			},
			cpp: {
				language: "cpp",
				sourceText: `template <typename T>
requires std::equality_comparable<T>
T require_eq(T x) {
    return x;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 803,
				},
			},
			csharp: {
				language: "csharp",
				sourceText: `static T RequireEq<T>(T x) where T : IEquatable<T> {
    return x;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 812,
				},
			},
			go: {
				language: "go",
				sourceText: `func requireEq[T comparable](x T) T {
    return x
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 819,
				},
			},
			java: {
				language: "java",
				sourceText: `static <T extends Comparable<T>> T requireEq(T x) {
    return x;
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 826,
				},
			},
			javascript: {
				language: "javascript",
				sourceText: `const requireEq = (x, eq) => {
    if (!eq(x, x)) throw new Error("not equal");
    return x;
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 833,
				},
			},
			musi: {
				language: "musi",
				sourceText: "let require_eq[T] (x : T) where T : Eq : T := x;",
				evidence: {
					path: "crates/music_sema/src/tests.rs",
					line: 901,
				},
			},
			python: {
				language: "python",
				sourceText: `from typing import TypeVar

T = TypeVar("T")

def require_eq(x: T) -> T:
    return x`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 849,
				},
			},
			rust: {
				language: "rust",
				sourceText: `fn require_eq<T>(x: T) -> T
where
    T: PartialEq,
{
    x
}`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 859,
				},
			},
			typescript: {
				language: "typescript",
				sourceText: `type Eq<T> = { equal(a: T, b: T): boolean };

const requireEq = <T>(x: T, _eq: Eq<T>): T => {
    return x;
};`,
				evidence: {
					path: "www/src/content/example-registry.ts",
					line: 866,
				},
			},
		},
	},
] as const;
