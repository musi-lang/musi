#pragma once

#include <atomic>
#include <string>

namespace musi {

  struct Object {
    std::atomic<uint32_t> m_refcount {1};

    void retain();
    void release();

    Object() = default;
    virtual ~Object() = default;

    Object(const Object&) = delete;
    Object(Object&&) = delete;
    auto operator=(const Object&) -> Object& = delete;
    auto operator=(Object&&) -> Object& = delete;
  };

  struct StringObject : Object {
    std::string data;

    explicit StringObject(std::string str) : data(std::move(str)) {}
  };

}  // namespace musi
