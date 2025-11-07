#include "vm.hpp"

#include <doctest/doctest.h>

TEST_CASE("VM executes 'LdcI4_0' and 'Ret'") {
  std::array<uint8_t, 2> bc {
      0x16,  // LdcI4_0
      0x2A   // Ret
  };
  musi::VM vm(bc, {}, {}, {});
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes basic addition") {
  std::array<uint8_t, 4> bc {
      0x17,  // LdcI4_1
      0x17,  // LdcI4_1
      0x58,  // Add
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM fails on 'division by zero'") {
  std::array<uint8_t, 4> bc {
      0x17,  // LdcI4_1
      0x16,  // LdcI4_0
      0x5B,  // Div
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK_FALSE(result.has_value());
}

TEST_CASE("VM executes subtraction") {
  std::array<uint8_t, 4> bc {
      0x1A,  // LdcI4_4
      0x18,  // LdcI4_2
      0x59,  // Sub
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes multiplication") {
  std::array<uint8_t, 4> bc {
      0x19,  // LdcI4_3
      0x1A,  // LdcI4_4
      0x5A,  // Mul
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes negation") {
  std::array<uint8_t, 3> bc {
      0x1B,  // LdcI4_5
      0x65,  // Neg
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpEq' (equal)") {
  std::array<uint8_t, 5> bc {
      0x18,  // LdcI4_2
      0x18,  // LdcI4_2
      0xFE,
      0x01,  // CmpEq
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpNe' (not equal)") {
  std::array<uint8_t, 5> bc {
      0x17,  // LdcI4_1
      0x18,  // LdcI4_2
      0xFE,
      0x02,  // CmpNe
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpLt' (less than)") {
  std::array<uint8_t, 5> bc {
      0x17,  // LdcI4_1
      0x18,  // LdcI4_2
      0xFE,
      0x03,  // CmpLt
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpGt' (greater than)") {
  std::array<uint8_t, 5> bc {
      0x19,  // LdcI4_3
      0x17,  // LdcI4_1
      0xFE,
      0x04,  // CmpGt
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpLe' (less or equal)") {
  std::array<uint8_t, 5> bc {
      0x18,  // LdcI4_2
      0x18,  // LdcI4_2
      0xFE,
      0x05,  // CmpLe
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'CmpGe' (greater or equal)") {
  std::array<uint8_t, 5> bc {
      0x1A,  // LdcI4_4
      0x19,  // LdcI4_3
      0xFE,
      0x06,  // CmpGe
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes unconditional branch") {
  std::array<uint8_t, 7> bc {
      0x38,
      0x04,
      0x00,  // Br +4 (skip next instruction)
      0x16,  // LdcI4_0 (skipped)
      0x17,  // LdcI4_1
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'BrTrue' when condition is 'true'") {
  std::array<uint8_t, 8> bc {
      0x17,  // LdcI4_1
      0x17,  // LdcI4_1
      0xFE,
      0x01,  // CmpEq (pushes true)
      0x39,
      0x01,
      0x00,  // BrTrue +1
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}

TEST_CASE("VM executes 'BrFalse' when condition is 'false'") {
  std::array<uint8_t, 8> bc {
      0x17,  // LdcI4_1
      0x18,  // LdcI4_2
      0xFE,
      0x01,  // CmpEq (pushes false)
      0x3A,
      0x01,
      0x00,  // BrFalse +1
      0x2A   // Ret
  };
  musi::VM vm(bc);
  const auto result = vm.exec();
  CHECK(result.has_value());
}
