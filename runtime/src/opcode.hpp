#pragma once

#include <cstdint>

namespace musi {

  // NOLINTBEGIN(readability-identifier-naming)
  enum class Opcode : uint8_t {
    Nop = 0x00,
    LdArg = 0x09,
    LdLoc = 0x0E,
    StLoc = 0x13,
    LdcI4M1 = 0x15,
    LdcI4_0 = 0x16,
    LdcI4_1 = 0x17,
    LdcI4_2 = 0x18,
    LdcI4_3 = 0x19,
    LdcI4_4 = 0x1A,
    LdcI4_5 = 0x1B,
    LdcI4_6 = 0x1C,
    LdcI4_7 = 0x1D,
    LdcI4_8 = 0x1E,
    LdcI4 = 0x20,
    LdcUnit = 0x21,
    Dup = 0x25,
    Pop = 0x26,
    Call = 0x28,
    Ret = 0x2A,
    Br = 0x38,
    BrTrue = 0x39,
    BrFalse = 0x3A,
    Add = 0x58,
    Sub = 0x59,
    Mul = 0x5A,
    Div = 0x5B,
    Neg = 0x65,
    CmpEq = 0xFE,
  };
  // NOLINTEND(readability-identifier-naming)

}  // namespace musi
