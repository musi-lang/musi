#include "object.hpp"

namespace musi {

  void Object::retain() {
    ++m_refcount;
  }

  void Object::release() {
    if (m_refcount.fetch_sub(1, std::memory_order_acq_rel) == 1) {
      delete this;
    }
  }

}  // namespace musi
