#pragma once

#include <functional>
#include <unordered_map>

#include "header.hpp"  // IWYU pragma: keep
#include "types.hpp"
#include "value.hpp"

namespace musi {

  class VM;
  using Intrinsic = std::function<void(VM&)>;

  struct Frame {
    size_t return_ip;
    ValueList saved_locals;
  };

  class VM {
    ValueList m_stack;
    ValueList m_locals;
    Bytecode m_bc;
    size_t m_ip {0};
    std::unordered_map<std::string, Intrinsic> m_intrinsics;
    ConstPool m_const_pool;
    ProcTable m_proc_table;
    std::unordered_map<uint32_t, std::string> m_link_table;
    std::vector<Frame> m_call_stack;

public:
    explicit VM(
        Bytecode bc,
        ConstPool const_pool = {},
        ProcTable proc_table = {},
        const LinkEntryList& link_entries = {});
    ~VM();

    void register_intrinsic(std::string name, Intrinsic fn);
    auto call_intrinsic(const std::string& name) -> Expected<void>;

    auto exec() -> Expected<void>;

    VM(VM&&) noexcept = default;
    auto operator=(VM&&) noexcept -> VM& = default;

    VM(const VM&) = delete;
    auto operator=(const VM&) -> VM& = delete;

    void push(const Value& val);
    auto pop() -> Value;

private:
    auto read_u16() -> uint16_t;
    auto read_i32() -> int32_t;

    template<typename T>
    auto pop_as() -> T;

    template<typename T>
    auto pop2() -> std::pair<T, T>;

    auto exec_ldcstr() -> Expected<void>;
    auto exec_ldloc() -> Expected<void>;
    auto exec_ldarg() -> Expected<void>;
    auto exec_stloc() -> Expected<void>;
    void exec_add();
    void exec_sub();
    void exec_mul();
    auto exec_div() -> Expected<void>;
    void exec_neg();
    auto exec_cmpeq() -> Expected<void>;
    void exec_br();
    void exec_brtrue();
    void exec_brfalse();
    auto exec_call() -> Expected<void>;
    auto exec_ret() -> Expected<void>;
  };

}  // namespace musi
