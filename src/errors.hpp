#pragma once

#include <format>

namespace musi::errors {
    static constexpr uint32_t MAX_CONSECUTIVE_ERRORS = 100;

    static constexpr auto exceeded(std::convertible_to<std::string_view> auto item) {
        return std::format("exceeded {}", item);
    }
    static constexpr auto expected(std::convertible_to<std::string_view> auto item) {
        return std::format("expected {}", item);
    }
    static constexpr auto expected_after(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} after {}", item, context);
    }
    static constexpr auto expected_at(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} at {}", item, context);
    }
    static constexpr auto expected_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} in {}", item, context);
    }
    static constexpr auto expected_of(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} of {}", item, context);
    }

    static constexpr auto invalid(std::convertible_to<std::string_view> auto item) {
        return std::format("invalid {}", item);
    }
    static constexpr auto invalid_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("invalid {} in {}", item, context);
    }

    static constexpr auto mismatched(std::convertible_to<std::string_view> auto item) {
        return std::format("mismatched {}", item);
    }
    static constexpr auto mixed(std::convertible_to<std::string_view> auto item) {
        return std::format("mixed {}", item);
    }

    static constexpr auto unexpected(std::convertible_to<std::string_view> auto item) {
        return std::format("unexpected {}", item);
    }
    static constexpr auto unexpected_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("unexpected {} in {}", item, context);
    }
    static constexpr auto unknown(std::convertible_to<std::string_view> auto item) {
        return std::format("unknown {}", item);
    }
    static constexpr auto unterminated(std::convertible_to<std::string_view> auto item) {
        return std::format("unterminated {}", item);
    }
}  // namespace musi::errors
