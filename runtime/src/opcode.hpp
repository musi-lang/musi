#pragma once

#include <cstdint>

namespace musi {

  // NOLINTBEGIN(readability-identifier-naming)
  enum class Opcode : uint8_t {
    Nop = 0x00,
    LdcI4 = 0x20,
    LdcI4M1 = 0x15,
    LdcUnit = 0x21,
    LdcStr = 0x22,
    LdLoc = 0x0E,
    StLoc = 0x13,
    LdArg = 0x09,
    Dup = 0x25,
    Pop = 0x26,
    Add = 0x58,
    Sub = 0x59,
    Mul = 0x5A,
    Div = 0x5B,
    Neg = 0x65,
    CmpEq = 0xFE,
    CmpNe = 0xFE,
    CmpLt = 0xFE,
    CmpGt = 0xFE,
    CmpLe = 0xFE,
    CmpGe = 0xFE,
    Br = 0x38,
    BrTrue = 0x39,
    BrFalse = 0x3A,
    Call = 0x28,
    Ret = 0x2A,
  };
  // NOLINTEND(readability-identifier-naming)

}  // namespace musi
