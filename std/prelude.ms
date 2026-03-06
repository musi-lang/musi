// std/prelude.ms -- standard definitions always in scope

#[intrinsic("int")]   export native opaque record Int {};
#[intrinsic("nat")]   export native opaque record Nat {};
#[intrinsic("float")] export native opaque record Float {};

#[intrinsic("int8")]  export native opaque record Int8 {};
#[intrinsic("int16")] export native opaque record Int16 {};
#[intrinsic("int32")] export native opaque record Int32 {};
#[intrinsic("int64")] export native opaque record Int64 {};

#[intrinsic("nat8")]  export native opaque record Nat8 {};
#[intrinsic("nat16")] export native opaque record Nat16 {};
#[intrinsic("nat32")] export native opaque record Nat32 {};
#[intrinsic("nat64")] export native opaque record Nat64 {};

#[intrinsic("float32")] export native opaque record Float32 {};
#[intrinsic("float64")] export native opaque record Float64 {};

#[intrinsic("rune")]   export native opaque record Rune {};   // Unicode scalar value; not a C char
#[intrinsic("string")] export native opaque record String {};

#[intrinsic("unit")] export native opaque record Unit {};

export choice Bool { False := 0 | True := 1 };

export choice Option['T] { None | Some('T) };  // no null pointers

export choice Expect['T, 'E] { Fail('E) | Pass('T) };  // explicit error handling; no exceptions

#[intrinsic("writeln")]       export native fn writeln(s: String): Unit;
#[intrinsic("write")]         export native fn write(s: String): Unit;
#[intrinsic("int_to_string")]   export native fn int_to_string(n: Int): String;
#[intrinsic("float_to_string")] export native fn float_to_string(n: Float): String;

// -- Type class declarations --------------------------------------------------

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

class Show['T] {
    fn show(a: 'T): String;
};

// -- Given instances for built-in types ---------------------------------------

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

given Show[Float] {
    fn show(a: Float): String => float_to_string(a);
};

given Show[String] {
    fn show(a: String): String => a;
};
