/// Early-return protocol for `try` expressions. The compiler desugars
/// `try expr` into `Propagate.extract(expr)`, which either continues
/// with the success value or performs early return via the `Throw` effect.
#[lang := "propagate"]
export class Propagate over ('F, 'T, 'E) {
    let extract(value: 'F) : 'T ~> under { Throw of 'E };
};

given Propagate of (Option of 'T, 'T, ()) {
    let extract(value: Option of 'T) : 'T ~> under { Throw of () };
};

given Propagate of (Result of ('T, 'E), 'T, 'E) {
    let extract(value: Result of ('T, 'E)) : 'T ~> under { Throw of 'E };
};
