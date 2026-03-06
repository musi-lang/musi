#[intrinsic("int")]   export extrin opaque record Int {};
#[intrinsic("nat")]   export extrin opaque record Nat {};
#[intrinsic("float")] export extrin opaque record Float {};

#[intrinsic("int8")]  export extrin opaque record Int8 {};
#[intrinsic("int16")] export extrin opaque record Int16 {};
#[intrinsic("int32")] export extrin opaque record Int32 {};
#[intrinsic("int64")] export extrin opaque record Int64 {};

#[intrinsic("nat8")]  export extrin opaque record Nat8 {};
#[intrinsic("nat16")] export extrin opaque record Nat16 {};
#[intrinsic("nat32")] export extrin opaque record Nat32 {};
#[intrinsic("nat64")] export extrin opaque record Nat64 {};

#[intrinsic("float32")] export extrin opaque record Float32 {};
#[intrinsic("float64")] export extrin opaque record Float64 {};

#[intrinsic("rune")]   export extrin opaque record Rune {};   // Unicode scalar value; not a C char
#[intrinsic("string")] export extrin opaque record String {};

#[intrinsic("unit")] export extrin opaque record Unit {};

export choice Bool { False := 0 | True := 1 };

export choice Option['T] { None | Some('T) };  // no null pointers

export choice Expect['T, 'E] { Fail('E) | Pass('T) };  // explicit error handling; no exceptions

#[intrinsic("writeln")]       export extrin fn writeln(s: String): Unit;
#[intrinsic("write")]         export extrin fn write(s: String): Unit;
#[intrinsic("int_to_string")]   export extrin fn int_to_string(n: Int): String;
#[intrinsic("float_to_string")] export extrin fn float_to_string(n: Float): String;

class Eq['T] {
    fn eq(a: 'T, b: 'T): Bool;
};

class Ord['T] satisfies Eq['T] {
    fn lt(a: 'T, b: 'T): Bool;
    fn gt(a: 'T, b: 'T): Bool;
    fn leq(a: 'T, b: 'T): Bool;
    fn geq(a: 'T, b: 'T): Bool;
    fn min(a: 'T, b: 'T): 'T;
    fn max(a: 'T, b: 'T): 'T;
};

class Add['T] {
    fn add(a: 'T, b: 'T): 'T;
};

class Sub['T] {
    fn sub(a: 'T, b: 'T): 'T;
};

class Mul['T] {
    fn mul(a: 'T, b: 'T): 'T;
};

class Div['T] {
    fn div(a: 'T, b: 'T): 'T;
};

class Rem['T] {
    fn rem(a: 'T, b: 'T): 'T;
};

class Show['T] {
    fn show(a: 'T): String;
};

given Eq[Int] {
    fn eq(a: Int, b: Int): Bool => a = b;
};

given Ord[Int] {
    fn lt(a: Int, b: Int): Bool => a < b;
    fn gt(a: Int, b: Int): Bool => a > b;
    fn leq(a: Int, b: Int): Bool => not (a > b);
    fn geq(a: Int, b: Int): Bool => not (a < b);
    fn min(a: Int, b: Int): Int => if a < b then a else b;
    fn max(a: Int, b: Int): Int => if a > b then a else b;
};

given Add[Int] {
    fn add(a: Int, b: Int): Int => a + b;
};

given Sub[Int] {
    fn sub(a: Int, b: Int): Int => a - b;
};

given Mul[Int] {
    fn mul(a: Int, b: Int): Int => a * b;
};

given Div[Int] {
    fn div(a: Int, b: Int): Int => a / b;
};

given Rem[Int] {
    fn rem(a: Int, b: Int): Int => a % b;
};

given Show[Int] {
    fn show(a: Int): String => int_to_string(a);
};

given Eq[Float] {
    fn eq(a: Float, b: Float): Bool => a = b;
};

given Ord[Float] {
    fn lt(a: Float, b: Float): Bool => a < b;
    fn gt(a: Float, b: Float): Bool => a > b;
    fn leq(a: Float, b: Float): Bool => not (a > b);
    fn geq(a: Float, b: Float): Bool => not (a < b);
    fn min(a: Float, b: Float): Float => if a < b then a else b;
    fn max(a: Float, b: Float): Float => if a > b then a else b;
};

given Add[Float] {
    fn add(a: Float, b: Float): Float => a + b;
};

given Div[Float] {
    fn div(a: Float, b: Float): Float => a / b;
};

given Eq[Bool] {
    fn eq(a: Bool, b: Bool): Bool => a = b;
};

given Show[Float] {
    fn show(a: Float): String => float_to_string(a);
};

given Show[String] {
    fn show(a: String): String => a;
};

#[intrinsic("array_length")]  export extrin fn array_length(arr: []Int): Int;
#[intrinsic("array_push")]    export extrin fn array_push(arr: []Int, val: Int): Unit;
#[intrinsic("array_pop")]     export extrin fn array_pop(arr: []Int): Option[Int];
#[intrinsic("array_get")]     export extrin fn array_get(arr: []Int, idx: Int): Int;
#[intrinsic("array_set")]     export extrin fn array_set(arr: []Int, idx: Int, val: Int): Unit;
#[intrinsic("array_slice")]   export extrin fn array_slice(arr: []Int, start: Int, end: Int): []Int;

class Iterable['C, 'E] {
    fn iter_len(c: 'C): Int;
    fn iter_get(c: 'C, i: Int): 'E;
};

given Iterable[[]'T, 'T] {
    fn iter_len(c: []'T): Int => array_length(c);
    fn iter_get(c: []'T, i: Int): 'T => array_get(c, i);
};

#[intrinsic("assert")]     export extrin fn assert(cond: Bool): Unit;
#[intrinsic("assert_msg")] export extrin fn assert_msg(cond: Bool, msg: String): Unit;
#[intrinsic("test")]       export extrin fn test(name: String, f: () -> Unit): Unit;
