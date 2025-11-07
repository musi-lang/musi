#include "vm.hpp"

#include <bit>
#include <format>

#include "object.hpp"
#include "opcode.hpp"

namespace musi {

  VM::VM(Bytecode bc) : m_bc(bc) {}

  auto VM::register_intrinsic(std::string name, Intrinsic fn) -> void {
    m_intrinsics[std::move(name)] = std::move(fn);
  }

  auto VM::call_intrinsic(const std::string& name) -> Expected<void> {
    const auto it = m_intrinsics.find(name);
    if (it == m_intrinsics.end()) {
      return std::unexpected(std::format("intrinsic '{}' not found", name));
    }
    it->second(*this);
    return {};
  }

  VM::~VM() {
    const auto release_if_object = [](const Value& val) {
      if (const auto* obj_ptr = std::get_if<Object*>(&val)) {
        if (*obj_ptr != nullptr) {
          (*obj_ptr)->release();
        }
      }
    };

    for (const auto& val : m_stack) {
      release_if_object(val);
    }
    for (const auto& val : m_locals) {
      release_if_object(val);
    }
  }

  void VM::push(const Value& val) {
    if (const auto* obj_ptr = std::get_if<Object*>(&val)) {
      if (*obj_ptr != nullptr) {
        (*obj_ptr)->retain();
      }
    }
    m_stack.push_back(val);
  }

  auto VM::pop() -> Value {
    const auto val = m_stack.back();
    m_stack.pop_back();
    if (const auto* obj_ptr = std::get_if<Object*>(&val)) {
      if (*obj_ptr != nullptr) {
        (*obj_ptr)->release();
      }
    }
    return val;
  }

  auto VM::read_u16() -> uint16_t {
    const auto lo = static_cast<uint16_t>(m_bc[m_ip++]);
    const auto hi = static_cast<uint16_t>(m_bc[m_ip++]);
    return lo | static_cast<uint16_t>(hi << 8U);
  }

  auto VM::read_i32() -> int32_t {
    const auto b0 = static_cast<uint32_t>(m_bc[m_ip++]);
    const auto b1 = static_cast<uint32_t>(m_bc[m_ip++]);
    const auto b2 = static_cast<uint32_t>(m_bc[m_ip++]);
    const auto b3 = static_cast<uint32_t>(m_bc[m_ip++]);
    const auto u32 = b0 | (b1 << 8U) | (b2 << 16U) | (b3 << 24U);
    return std::bit_cast<int32_t>(u32);
  }

  template<typename T>
  auto VM::pop_as() -> T {
    return std::get<T>(pop());
  }

  template<typename T>
  auto VM::pop2() -> std::pair<T, T> {
    const auto b = pop_as<T>();
    const auto a = pop_as<T>();
    return {a, b};
  }

  auto VM::exec() -> Expected<void> {
    if (m_bc.empty()) {
      return std::unexpected("empty bytecode");
    }

    while (m_ip < m_bc.size()) {
      const auto opcode = static_cast<Opcode>(m_bc[m_ip++]);
      switch (opcode) {
        case Opcode::Nop:
          break;
        case Opcode::LdcI4M1:
          push(-1);
          break;
        case Opcode::LdcI4_0:
          push(0);
          break;
        case Opcode::LdcI4_1:
          push(1);
          break;
        case Opcode::LdcI4_2:
          push(2);
          break;
        case Opcode::LdcI4_3:
          push(3);
          break;
        case Opcode::LdcI4_4:
          push(4);
          break;
        case Opcode::LdcI4_5:
          push(5);
          break;
        case Opcode::LdcI4_6:
          push(6);
          break;
        case Opcode::LdcI4_7:
          push(7);
          break;
        case Opcode::LdcI4_8:
          push(8);
          break;
        case Opcode::LdcI4: {
          const auto val = read_i32();
          push(val);
          break;
        }
        case Opcode::LdcUnit:
          push(Unit {});
          break;
        case Opcode::LdLoc: {
          const auto idx = read_u16();
          if (idx >= m_locals.size()) {
            return std::unexpected("local index out of bounds");
          }
          push(m_locals[idx]);
          break;
        }
        case Opcode::StLoc: {
          const auto idx = read_u16();
          if (idx >= m_locals.size()) {
            m_locals.resize(idx + 1, Unit {});
          }
          m_locals[idx] = pop();
          break;
        }
        case Opcode::Add: {
          const auto [a, b] = pop2<int32_t>();
          push(a + b);
          break;
        }
        case Opcode::Sub: {
          const auto [a, b] = pop2<int32_t>();
          push(a - b);
          break;
        }
        case Opcode::Mul: {
          const auto [a, b] = pop2<int32_t>();
          push(a * b);
          break;
        }
        case Opcode::Div: {
          const auto [a, b] = pop2<int32_t>();
          if (b == 0) {
            return std::unexpected("division by zero");
          }
          push(a / b);
          break;
        }
        case Opcode::Neg: {
          const auto a = pop_as<int32_t>();
          push(-a);
          break;
        }
        case Opcode::CmpEq: {
          const auto sub = m_bc[m_ip++];
          const auto [a, b] = pop2<int32_t>();
          switch (sub) {
            case 0x01:
              push(a == b);
              break;
            case 0x02:
              push(a != b);
              break;
            case 0x03:
              push(a < b);
              break;
            case 0x04:
              push(a > b);
              break;
            case 0x05:
              push(a <= b);
              break;
            case 0x06:
              push(a >= b);
              break;
            default:
              return std::unexpected(
                  std::format("invalid comparison sub-opcode: 0x{:02X}", sub));
          }
          break;
        }
        case Opcode::Br: {
          const auto offset = read_i32();
          m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
          break;
        }
        case Opcode::BrTrue: {
          const auto offset = read_i32();
          const auto cond = pop_as<bool>();
          if (cond) {
            m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
          }
          break;
        }
        case Opcode::BrFalse: {
          const auto offset = read_i32();
          const auto cond = pop_as<bool>();
          if (!cond) {
            m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
          }
          break;
        }
        case Opcode::Ret:
          return {};
        default:
          return std::unexpected(std::format(
              "unhandled opcode: 0x{:02X}",
              static_cast<uint8_t>(opcode)));
      }
    }

    return {};
  }

}  // namespace musi
