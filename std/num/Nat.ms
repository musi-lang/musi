#[intrinsic("nat_to_string")] export extrin fn nat_to_string(n: Nat): String;

export fn nat_min(a: Nat, b: Nat): Nat => if a < b then a else b;
export fn nat_max(a: Nat, b: Nat): Nat => if a > b then a else b;
export fn nat_clamp(n: Nat, lo: Nat, hi: Nat): Nat => nat_min(nat_max(n, lo), hi);
