/// Total ordering. Instances must satisfy antisymmetry, transitivity,
/// and totality. Requires `Eq` as a superclass.
export class Ord over 'T where 'T <: Eq {
    let compare(a: 'T, b: 'T) : Ordering;
    let (<)(a: 'T, b: 'T) : Bool := match compare(a, b) (.Lt => .True | _ => .False);
    let (<=)(a: 'T, b: 'T) : Bool := match compare(a, b) (.Gt => .False | _ => .True);
    let (>)(a: 'T, b: 'T) : Bool := match compare(a, b) (.Gt => .True | _ => .False);
    let (>=)(a: 'T, b: 'T) : Bool := match compare(a, b) (.Lt => .False | _ => .True);
    let min(a: 'T, b: 'T) : 'T := (a if a <= b | b if _);
    let max(a: 'T, b: 'T) : 'T := (a if a >= b | b if _);

    law antisymmetry := forall a b -> (a <= b) and (b <= a) = (a = b);
    law transitivity := forall a b c -> (a <= b) and (b <= c) = (a <= c);
    law totality := forall a b -> (a <= b) or (b <= a)
};

given Ord of Int { let compare(a: Int, b: Int) : Ordering };
given Ord of Float64 { let compare(a: Float64, b: Float64) : Ordering };
given Ord of String { let compare(a: String, b: String) : Ordering };
given Ord of Rune { let compare(a: Rune, b: Rune) : Ordering };
