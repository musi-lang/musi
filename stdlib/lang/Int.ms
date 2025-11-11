export const Int := record {
  /* compiler intrinsic */
};

export const Int.abs := proc (n: Int) -> Int {
  if n < 0 then -n else n
};
