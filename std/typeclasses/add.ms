/// Numeric addition.
///
/// Instances should satisfy commutativity and associativity.
export class Add over 'T {
    let (+)(a: 'T, b: 'T) : 'T;

    law commutativity := (a + b) = (b + a);
    law associativity := ((a + b) + c) = (a + (b + c));
};

/// Numeric subtraction.
///
/// Provides the binary `-` operator.
export class Sub over 'T {
    let (-)(a: 'T, b: 'T) : 'T;
};

/// Numeric multiplication.
///
/// Instances should satisfy commutativity and associativity.
export class Mul over 'T {
    let (*)(a: 'T, b: 'T) : 'T;

    law commutativity := (a * b) = (b * a);
    law associativity := ((a * b) * c) = (a * (b * c));
};

/// Numeric division.
///
/// Provides the binary `/` operator.
export class Div over 'T {
    let (/)(a: 'T, b: 'T) : 'T;
};

/// Remainder after division.
///
/// Provides the binary `%` operator.
export class Rem over 'T {
    let (%)(a: 'T, b: 'T) : 'T;
};

/// Unary negation.
///
/// Provides the `negate` function for computing the additive inverse.
export class Neg over 'T {
    let negate(value: 'T) : 'T;
};

given Add of Int { let (+)(a: Int, b: Int) : Int; };
given Sub of Int { let (-)(a: Int, b: Int) : Int; };
given Mul of Int { let (*)(a: Int, b: Int) : Int; };
given Div of Int { let (/)(a: Int, b: Int) : Int; };
given Rem of Int { let (%)(a: Int, b: Int) : Int; };
given Neg of Int { let negate(value: Int) : Int; };

given Add of Float64 { let (+)(a: Float64, b: Float64) : Float64; };
given Sub of Float64 { let (-)(a: Float64, b: Float64) : Float64; };
given Mul of Float64 { let (*)(a: Float64, b: Float64) : Float64; };
given Div of Float64 { let (/)(a: Float64, b: Float64) : Float64; };
given Rem of Float64 { let (%)(a: Float64, b: Float64) : Float64; };
given Neg of Float64 { let negate(value: Float64) : Float64; };

/// String concatenation via `+`.
given Add of String { let (+)(a: String, b: String) : String; };
