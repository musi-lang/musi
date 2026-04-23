let core := import "musi:core";

--- Fixed-width bit-vector type constructor.
export let Bits := core.Bits;

--- Build zero-valued 1-bit bits.
export let zero1 () : Bits[1] := 0 :?> Bits[1];
--- Build zero-valued 8-bit bits.
export let zero8 () : Bits[8] := 0 :?> Bits[8];
--- Build zero-valued 16-bit bits.
export let zero16 () : Bits[16] := 0 :?> Bits[16];
--- Build zero-valued 32-bit bits.
export let zero32 () : Bits[32] := 0 :?> Bits[32];
--- Build zero-valued 64-bit bits.
export let zero64 () : Bits[64] := 0 :?> Bits[64];

--- Build all-ones 1-bit bits.
export let ones1 () : Bits[1] := not zero1();
--- Build all-ones 8-bit bits.
export let ones8 () : Bits[8] := not zero8();
--- Build all-ones 16-bit bits.
export let ones16 () : Bits[16] := not zero16();
--- Build all-ones 32-bit bits.
export let ones32 () : Bits[32] := not zero32();
--- Build all-ones 64-bit bits.
export let ones64 () : Bits[64] := not zero64();

--- Convert natural number to 1-bit bits.
export let fromNat1 (value : Nat) : Bits[1] := value :?> Bits[1];
--- Convert natural number to 8-bit bits.
export let fromNat8 (value : Nat) : Bits[8] := value :?> Bits[8];
--- Convert natural number to 16-bit bits.
export let fromNat16 (value : Nat) : Bits[16] := value :?> Bits[16];
--- Convert natural number to 32-bit bits.
export let fromNat32 (value : Nat) : Bits[32] := value :?> Bits[32];
--- Convert natural number to 64-bit bits.
export let fromNat64 (value : Nat) : Bits[64] := value :?> Bits[64];

--- Convert integer to 1-bit bits.
export let fromInt1 (value : Int) : Bits[1] := value :?> Bits[1];
--- Convert integer to 8-bit bits.
export let fromInt8 (value : Int) : Bits[8] := value :?> Bits[8];
--- Convert integer to 16-bit bits.
export let fromInt16 (value : Int) : Bits[16] := value :?> Bits[16];
--- Convert integer to 32-bit bits.
export let fromInt32 (value : Int) : Bits[32] := value :?> Bits[32];
--- Convert integer to 64-bit bits.
export let fromInt64 (value : Int) : Bits[64] := value :?> Bits[64];

--- Convert 1-bit bits to natural number.
export let toNat1 (value : Bits[1]) : Nat := value :?> Nat;
--- Convert 8-bit bits to natural number.
export let toNat8 (value : Bits[8]) : Nat := value :?> Nat;
--- Convert 16-bit bits to natural number.
export let toNat16 (value : Bits[16]) : Nat := value :?> Nat;
--- Convert 32-bit bits to natural number.
export let toNat32 (value : Bits[32]) : Nat := value :?> Nat;
--- Convert 64-bit bits to natural number.
export let toNat64 (value : Bits[64]) : Nat := value :?> Nat;

--- Convert 1-bit bits to integer.
export let toInt1 (value : Bits[1]) : Int := value :?> Int;
--- Convert 8-bit bits to integer.
export let toInt8 (value : Bits[8]) : Int := value :?> Int;
--- Convert 16-bit bits to integer.
export let toInt16 (value : Bits[16]) : Int := value :?> Int;
--- Convert 32-bit bits to integer.
export let toInt32 (value : Bits[32]) : Int := value :?> Int;
--- Convert 64-bit bits to integer.
export let toInt64 (value : Bits[64]) : Int := value :?> Int;

--- Return true when 1-bit bit-vectors are equal.
export let equals1 (left : Bits[1], right : Bits[1]) : Bool := left = right;

--- Return true when 1-bit bit-vectors differ.
export let notEquals1 (left : Bits[1], right : Bits[1]) : Bool := left /= right;

--- Combine 1-bit bit-vectors with logical and.
export let logicalAnd1 (left : Bits[1], right : Bits[1]) : Bits[1] :=
  left and right;

--- Combine 1-bit bit-vectors with logical or.
export let logicalOr1 (left : Bits[1], right : Bits[1]) : Bits[1] :=
  left or right;

--- Combine 1-bit bit-vectors with logical xor.
export let logicalXor1 (left : Bits[1], right : Bits[1]) : Bits[1] :=
  left xor right;

--- Invert every 1-bit bit.
export let logicalNot1 (value : Bits[1]) : Bits[1] := not value;

--- Return true when 8-bit bit-vectors are equal.
export let equals8 (left : Bits[8], right : Bits[8]) : Bool := left = right;

--- Return true when 8-bit bit-vectors differ.
export let notEquals8 (left : Bits[8], right : Bits[8]) : Bool := left /= right;

--- Combine 8-bit bit-vectors with logical and.
export let logicalAnd8 (left : Bits[8], right : Bits[8]) : Bits[8] :=
  left and right;

--- Combine 8-bit bit-vectors with logical or.
export let logicalOr8 (left : Bits[8], right : Bits[8]) : Bits[8] :=
  left or right;

--- Combine 8-bit bit-vectors with logical xor.
export let logicalXor8 (left : Bits[8], right : Bits[8]) : Bits[8] :=
  left xor right;

--- Invert every 8-bit bit.
export let logicalNot8 (value : Bits[8]) : Bits[8] := not value;

--- Return true when 16-bit bit-vectors are equal.
export let equals16 (left : Bits[16], right : Bits[16]) : Bool := left = right;

--- Return true when 16-bit bit-vectors differ.
export let notEquals16 (left : Bits[16], right : Bits[16]) : Bool :=
  left /= right;

--- Combine 16-bit bit-vectors with logical and.
export let logicalAnd16 (left : Bits[16], right : Bits[16]) : Bits[16] :=
  left and right;

--- Combine 16-bit bit-vectors with logical or.
export let logicalOr16 (left : Bits[16], right : Bits[16]) : Bits[16] :=
  left or right;

--- Combine 16-bit bit-vectors with logical xor.
export let logicalXor16 (left : Bits[16], right : Bits[16]) : Bits[16] :=
  left xor right;

--- Invert every 16-bit bit.
export let logicalNot16 (value : Bits[16]) : Bits[16] := not value;

--- Return true when 32-bit bit-vectors are equal.
export let equals32 (left : Bits[32], right : Bits[32]) : Bool := left = right;

--- Return true when 32-bit bit-vectors differ.
export let notEquals32 (left : Bits[32], right : Bits[32]) : Bool :=
  left /= right;

--- Combine 32-bit bit-vectors with logical and.
export let logicalAnd32 (left : Bits[32], right : Bits[32]) : Bits[32] :=
  left and right;

--- Combine 32-bit bit-vectors with logical or.
export let logicalOr32 (left : Bits[32], right : Bits[32]) : Bits[32] :=
  left or right;

--- Combine 32-bit bit-vectors with logical xor.
export let logicalXor32 (left : Bits[32], right : Bits[32]) : Bits[32] :=
  left xor right;

--- Invert every 32-bit bit.
export let logicalNot32 (value : Bits[32]) : Bits[32] := not value;

--- Return true when 64-bit bit-vectors are equal.
export let equals64 (left : Bits[64], right : Bits[64]) : Bool := left = right;

--- Return true when 64-bit bit-vectors differ.
export let notEquals64 (left : Bits[64], right : Bits[64]) : Bool :=
  left /= right;

--- Combine 64-bit bit-vectors with logical and.
export let logicalAnd64 (left : Bits[64], right : Bits[64]) : Bits[64] :=
  left and right;

--- Combine 64-bit bit-vectors with logical or.
export let logicalOr64 (left : Bits[64], right : Bits[64]) : Bits[64] :=
  left or right;

--- Combine 64-bit bit-vectors with logical xor.
export let logicalXor64 (left : Bits[64], right : Bits[64]) : Bits[64] :=
  left xor right;

--- Invert every 64-bit bit.
export let logicalNot64 (value : Bits[64]) : Bits[64] := not value;

--- Return true when 1-bit bits are zero.
export let isZero1 (value : Bits[1]) : Bool := value = zero1();

--- Return true when any 1-bit bit is set.
export let any1 (value : Bits[1]) : Bool := value /= zero1();

--- Return true when every 1-bit bit is set.
export let all1 (value : Bits[1]) : Bool := value = ones1();

--- Return true when 8-bit bits are zero.
export let isZero8 (value : Bits[8]) : Bool := value = zero8();

--- Return true when any 8-bit bit is set.
export let any8 (value : Bits[8]) : Bool := value /= zero8();

--- Return true when every 8-bit bit is set.
export let all8 (value : Bits[8]) : Bool := value = ones8();

--- Return true when 16-bit bits are zero.
export let isZero16 (value : Bits[16]) : Bool := value = zero16();

--- Return true when any 16-bit bit is set.
export let any16 (value : Bits[16]) : Bool := value /= zero16();

--- Return true when every 16-bit bit is set.
export let all16 (value : Bits[16]) : Bool := value = ones16();

--- Return true when 32-bit bits are zero.
export let isZero32 (value : Bits[32]) : Bool := value = zero32();

--- Return true when any 32-bit bit is set.
export let any32 (value : Bits[32]) : Bool := value /= zero32();

--- Return true when every 32-bit bit is set.
export let all32 (value : Bits[32]) : Bool := value = ones32();

--- Return true when 64-bit bits are zero.
export let isZero64 (value : Bits[64]) : Bool := value = zero64();

--- Return true when any 64-bit bit is set.
export let any64 (value : Bits[64]) : Bool := value /= zero64();

--- Return true when every 64-bit bit is set.
export let all64 (value : Bits[64]) : Bool := value = ones64();
