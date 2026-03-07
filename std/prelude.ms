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
#[intrinsic("string_concat")]   export extrin fn string_concat(a: String, b: String): String;

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

#[intrinsic("array_length")]  export extrin fn array_length['T](arr: []'T): Int;
#[intrinsic("array_push")]    export extrin fn array_push['T](arr: []'T, val: 'T): Unit;
#[intrinsic("array_pop")]     export extrin fn array_pop['T](arr: []'T): Option['T];
#[intrinsic("array_get")]     export extrin fn array_get['T](arr: []'T, idx: Int): 'T;
#[intrinsic("array_set")]     export extrin fn array_set['T](arr: []'T, idx: Int, val: 'T): Unit;
#[intrinsic("array_slice")]   export extrin fn array_slice['T](arr: []'T, start: Int, end: Int): []'T;

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

// -- string core (musi:string) --
#[intrinsic("string_length")]   export extrin fn string_length(s: String): Int;
#[intrinsic("string_slice")]    export extrin fn string_slice(s: String, start: Int, end: Int): String;
#[intrinsic("string_to_int")]   export extrin fn string_to_int(s: String): Option[Int];
#[intrinsic("string_contains")] export extrin fn string_contains(s: String, sub: String): Bool;
#[intrinsic("nat_to_string")]   export extrin fn nat_to_string(n: Nat): String;

// -- string extended --
#[intrinsic("string_split")]    export extrin fn string_split(s: String, delim: String): []String;
#[intrinsic("string_trim")]     export extrin fn string_trim(s: String): String;
#[intrinsic("string_to_lower")] export extrin fn string_to_lower(s: String): String;
#[intrinsic("string_to_upper")] export extrin fn string_to_upper(s: String): String;
#[intrinsic("string_to_float")] export extrin fn string_to_float(s: String): Option[Float];
#[intrinsic("string_index_of")] export extrin fn string_index_of(s: String, sub: String): Option[Int];

// -- numeric casts --
#[intrinsic("int_to_float")] export extrin fn int_to_float(n: Int): Float;
#[intrinsic("float_to_int")] export extrin fn float_to_int(f: Float): Int;

// -- math trig (musi:math) --
#[intrinsic("sin")]   export extrin fn sin(x: Float): Float;
#[intrinsic("cos")]   export extrin fn cos(x: Float): Float;
#[intrinsic("tan")]   export extrin fn tan(x: Float): Float;
#[intrinsic("atan2")] export extrin fn atan2(y: Float, x: Float): Float;
#[intrinsic("log")]   export extrin fn log(x: Float): Float;
#[intrinsic("exp")]   export extrin fn exp(x: Float): Float;

// -- map (keys must be Int or String at runtime) --
export opaque record Map['K, 'V] {};
#[intrinsic("map_new")]    export extrin fn map_new['K, 'V](): Map['K, 'V];
#[intrinsic("map_get")]    export extrin fn map_get['K, 'V](m: Map['K, 'V], key: 'K): Option['V];
#[intrinsic("map_set")]    export extrin fn map_set['K, 'V](m: Map['K, 'V], key: 'K, val: 'V): Unit;
#[intrinsic("map_has")]    export extrin fn map_has['K, 'V](m: Map['K, 'V], key: 'K): Bool;
#[intrinsic("map_delete")] export extrin fn map_delete['K, 'V](m: Map['K, 'V], key: 'K): Unit;
#[intrinsic("map_keys")]   export extrin fn map_keys['K, 'V](m: Map['K, 'V]): []'K;
#[intrinsic("map_values")] export extrin fn map_values['K, 'V](m: Map['K, 'V]): []'V;
#[intrinsic("map_len")]    export extrin fn map_len['K, 'V](m: Map['K, 'V]): Int;
