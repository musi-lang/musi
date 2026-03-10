/// Convert a value to its human-readable string representation.
export class Show over 'T {
    let show(value: 'T) : String;
};

given Show of Int { let show(value: Int) : String; };
given Show of Float64 { let show(value: Float64) : String; };
given Show of Rune { let show(value: Rune) : String; };
given Show of Bool {
    let show(value: Bool) : String :=
        match value (.True => "True" | .False => "False");
};
given Show of String { let show(value: String) : String := value; };
given Show of () { let show(value: ()) : String := "()"; };
