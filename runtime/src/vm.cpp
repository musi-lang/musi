#include "vm.hpp"

#include <bit>
#include <format>
#include <print>

#include "object.hpp"
#include "opcode.hpp"

namespace musi {

  VM::VM(
      Bytecode bc,
      ConstPool const_pool,
      ProcTable proc_table,
      const LinkEntryList& link_entries)
      : m_bc(bc)
      , m_const_pool(std::move(const_pool))
      , m_proc_table(std::move(proc_table)) {
    for (const auto& entry : link_entries) {
      m_link_table[entry.proc_id] = entry.link_key;
    }
  }

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

  auto VM::exec_ldcstr() -> Expected<void> {
    const auto idx = read_u16();
    if (idx >= m_const_pool.size()) {
      return std::unexpected("const pool index out of bounds");
    }
    auto* str_obj = new StringObject(m_const_pool[idx]);
    push(static_cast<Object*>(str_obj));
    return {};
  }

  auto VM::exec_ldloc() -> Expected<void> {
    const auto idx = read_u16();
    if (idx >= m_locals.size()) {
      return std::unexpected("local index out of bounds");
    }
    push(m_locals[idx]);
    return {};
  }

  auto VM::exec_stloc() -> Expected<void> {
    const auto idx = read_u16();
    if (idx >= m_locals.size()) {
      m_locals.resize(idx + 1, Unit {});
    }
    m_locals[idx] = pop();
    return {};
  }

  auto VM::exec_cmpeq() -> Expected<void> {
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
            std::format("invalid comparison sub-opcode '0x{:02X}'", sub));
    }
    return {};
  }

  auto VM::exec_call() -> Expected<void> {
    const auto proc_id = static_cast<uint32_t>(read_i32());
    std::println(
        "[VM] call proc_id={} (table_size={})",
        proc_id,
        m_proc_table.size());
    if (proc_id >= m_proc_table.size()) {
      return std::unexpected(std::format("invalid procedure {}", proc_id));
    }
    const auto& proc = m_proc_table[proc_id];
    std::println(
        "[VM] proc.is_extern={{}} proc.bytecode_offset={{:#06x}}",
        proc.is_extern,
        proc.bytecode_offset);
    if (proc.is_extern) {
      const auto it = m_link_table.find(proc_id);
      if (it == m_link_table.end()) {
        return std::unexpected(
            std::format("no link key for 'extern' procedure {}", proc_id));
      }
      return call_intrinsic(it->second);
    }
    m_call_stack.push_back({m_ip, m_locals});
    m_locals.clear();
    m_ip = proc.bytecode_offset;
    return {};
  }

  auto VM::exec_ret() -> Expected<void> {
    if (m_call_stack.empty()) {
      return {};
    }
    const auto frame = m_call_stack.back();
    m_call_stack.pop_back();
    m_ip = frame.return_ip;
    m_locals = frame.saved_locals;
    return {};
  }

  void VM::exec_add() {
    const auto [a, b] = pop2<int32_t>();
    push(a + b);
  }

  void VM::exec_sub() {
    const auto [a, b] = pop2<int32_t>();
    push(a - b);
  }

  void VM::exec_mul() {
    const auto [a, b] = pop2<int32_t>();
    push(a * b);
  }

  auto VM::exec_div() -> Expected<void> {
    const auto [a, b] = pop2<int32_t>();
    if (b == 0) {
      return std::unexpected("division by zero");
    }
    push(a / b);
    return {};
  }

  void VM::exec_neg() {
    const auto a = pop_as<int32_t>();
    push(-a);
  }

  void VM::exec_br() {
    const auto offset = read_i32();
    m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
  }

  void VM::exec_brtrue() {
    const auto offset = read_i32();
    const auto cond = pop_as<bool>();
    if (cond) {
      m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
    }
  }

  void VM::exec_brfalse() {
    const auto offset = read_i32();
    const auto cond = pop_as<bool>();
    if (!cond) {
      m_ip = static_cast<size_t>(static_cast<int64_t>(m_ip) + offset);
    }
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
        case Opcode::LdcI4: {
          const auto val = read_i32();
          push(val);
          break;
        }
        case Opcode::LdcUnit:
          push(Unit {});
          break;
        case Opcode::LdcStr:
          if (const auto result = exec_ldcstr(); !result) {
            return result;
          }
          break;
        case Opcode::LdLoc:
          if (const auto result = exec_ldloc(); !result) {
            return result;
          }
          break;
        case Opcode::StLoc:
          if (const auto result = exec_stloc(); !result) {
            return result;
          }
          break;
        case Opcode::Add:
          exec_add();
          break;
        case Opcode::Sub:
          exec_sub();
          break;
        case Opcode::Mul:
          exec_mul();
          break;
        case Opcode::Div:
          if (const auto result = exec_div(); !result) {
            return result;
          }
          break;
        case Opcode::Neg:
          exec_neg();
          break;
        case Opcode::CmpEq:
          if (const auto result = exec_cmpeq(); !result) {
            return result;
          }
          break;
        case Opcode::Br:
          exec_br();
          break;
        case Opcode::BrTrue:
          exec_brtrue();
          break;
        case Opcode::BrFalse:
          exec_brfalse();
          break;
        case Opcode::Call:
          if (const auto result = exec_call(); !result) {
            return result;
          }
          break;
        case Opcode::Pop:
          pop();
          break;
        case Opcode::Ret:
          if (const auto result = exec_ret(); !result) {
            return result;
          }
          break;
        default:
          return std::unexpected(std::format(
              "unhandled opcode '0x{:02X}'",
              static_cast<uint8_t>(opcode)));
      }
    }

    return {};
  }

}  // namespace musi
