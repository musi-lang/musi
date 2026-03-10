/// Conversion from one type to another. Use `into(x)` to convert a value.
///
/// Example:
///   let x : Float64 := into(42)
export class Into over ('A, 'B) {
    let into(value: 'A) : 'B;
};

given Into of (Int, Float64) { let into(value: Int) : Float64; };
given Into of (Int, String) { let into(value: Int) : String; };
given Into of (Float64, String) { let into(value: Float64) : String; };
given Into of (Float64, Int) { let into(value: Float64) : Int; };
given Into of (Bool, String) { let into(value: Bool) : String; };
given Into of (Rune, String) { let into(value: Rune) : String; };
