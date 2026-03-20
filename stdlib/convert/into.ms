//! Into type class.

/// Conversion between types: implement to enable lossless conversion from 'a to 'b.
#[lang := "Into"]
export class Into ['a, 'b] {
  let into(a) : 'b;
};
