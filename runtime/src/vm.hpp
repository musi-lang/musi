#pragma once

#include <functional>
#include <unordered_map>

#include "types.hpp"
#include "value.hpp"

namespace musi {

  class VM;
  using Intrinsic = std::function<void(VM&)>;

  class VM {
    ValueList m_stack;
    ValueList m_locals;
    Bytecode m_bc;
    size_t m_ip {0};
    std::unordered_map<std::string, Intrinsic> m_intrinsics;

public:
    explicit VM(Bytecode bc);
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
  };

}  // namespace musi
