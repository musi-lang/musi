/// Structural equality. Instances must satisfy reflexivity, symmetry,
/// and transitivity.
export class Eq over 'T {
    let (=)(a: 'T, b: 'T) : Bool;
    let (/=)(a: 'T, b: 'T) : Bool := not (a = b);

    law reflexivity := a = a;
    law symmetry := (a = b) = (b = a);
    law transitivity := (a = b) and (b = c) = (a = c);
};

given Eq of Int { let (=)(a: Int, b: Int) : Bool; };
given Eq of Bool { let (=)(a: Bool, b: Bool) : Bool; };
given Eq of String { let (=)(a: String, b: String) : Bool; };
given Eq of Rune { let (=)(a: Rune, b: Rune) : Bool; };
given Eq of Float64 { let (=)(a: Float64, b: Float64) : Bool; };
given Eq of () { let (=)(a: (), b: ()) : Bool := .True; };
