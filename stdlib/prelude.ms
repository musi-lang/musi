//! Musi standard prelude — type classes, core types, primitive instances.

class Eq ['a] {
  let (=)(a, b) : Bool;

  law reflexivity(x) := x = x;
  law symmetry(x, y) := (x = y) = (y = x);
};

class Ord ['a] where 'a <: Eq {
  let compare(a, b) : Ordering;
};

class Show ['a] {
  let show(a) : String;
};

class Add ['a] {
  let (+)(a, b) : 'a;
};

class Into ['a, 'b] {
  let into(a) : 'b;
};

class Iterable ['a, 'elem] {
  let next(iter) : ?'elem;
};

class Propagate ['e] {
  let propagate(e) : Never;
};

// ── Core choice types ────────────────────────────────────────

export let Ordering := choice { Less + Equal + Greater };
export let Option ['a] := choice { None + Some of 'a };
export let Result ['a, 'e] := choice { Ok of 'a + Err of 'e };

// ── Instances: Eq ────────────────────────────────────────────

instance Eq of Int {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Float64 {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of String {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Bool {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Rune {
  let (=)(a, b) : Bool := a = b;
};

// ── Instances: Ord ───────────────────────────────────────────

instance Ord of Int {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

instance Ord of String {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

instance Ord of Rune {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

// ── Instances: Show ──────────────────────────────────────────

instance Show of Int {
  let show(a) : String := show(a);
};

instance Show of Float64 {
  let show(a) : String := show(a);
};

instance Show of Bool {
  let show(a) : String := show(a);
};

instance Show of String {
  let show(a) : String := a;
};

instance Show of Rune {
  let show(a) : String := show(a);
};

// ── Instances: Add ───────────────────────────────────────────

instance Add of Int {
  let (+)(a, b) : Int := a + b;
};

instance Add of Float64 {
  let (+)(a, b) : Float64 := a + b;
};

instance Add of String {
  let (+)(a, b) : String := a + b;
};

// ── Option helpers ───────────────────────────────────────────

export let unwrap_or ['a] := (opt, default) =>
  match opt (
    .Some(v) => v
    | .None => default
  );

export let map_option ['a, 'b] := (opt, f) =>
  match opt (
    .Some(v) => .Some(f(v))
    | .None => .None
  );

export let and_then ['a, 'b] := (opt, f) =>
  match opt (
    .Some(v) => f(v)
    | .None => .None
  );

// ── Result helpers ───────────────────────────────────────────

export let map_ok ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => .Ok(f(v))
    | .Err(e) => .Err(e)
  );

export let map_err ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => .Err(f(e))
  );

export let unwrap_ok ['a, 'e] := (res, default) =>
  match res (
    .Ok(v) => v
    | .Err(_) => default
  );
