let pass := 0 = 0;
let fail_bool (message : String) := 0 = 1;

export let fail (message : String) := fail_bool(message);

export let to_be (actual : Int, expected : Int) :=
    case () of (
        | _ if actual = expected => pass
        | _ => fail("expected values to be equal")
    );

export let to_equal (actual : Int, expected : Int) :=
    to_be(actual, expected);

export let to_be_truthy (actual : Bool) :=
    case () of (
        | _ if actual => pass
        | _ => fail("expected value to be truthy")
    );

export let to_be_falsy (actual : Bool) :=
    case () of (
        | _ if actual => fail("expected value to be falsy")
        | _ => pass
    );

export let to_be_greater_than (actual : Int, bound : Int) :=
    case () of (
        | _ if actual > bound => pass
        | _ => fail("expected value to be greater than bound")
    );

export let to_be_less_than (actual : Int, bound : Int) :=
    case () of (
        | _ if actual < bound => pass
        | _ => fail("expected value to be less than bound")
    );

export let expect (actual : Int) := (
    let match_to_be (expected : Int) := to_be(actual, expected);
    let match_to_equal (expected : Int) := to_equal(actual, expected);
    let match_to_be_greater_than (bound : Int) :=
        to_be_greater_than(actual, bound);
    let match_to_be_less_than (bound : Int) := to_be_less_than(actual, bound);
    {
        to_be := match_to_be,
        to_equal := match_to_equal,
        to_be_greater_than := match_to_be_greater_than,
        to_be_less_than := match_to_be_less_than
    }
);
