#pragma once

#include <format>

namespace musi::errors {
    static constexpr uint32_t MAX_CONSECUTIVE_ERRORS = 100;

    inline auto duplicate(std::convertible_to<std::string_view> auto item) {
        return std::format("duplicate {}", item);
    }
    inline auto duplicate_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("duplicate {} in {}", item, context);
    }

    inline auto exceeded(std::convertible_to<std::string_view> auto item) {
        return std::format("exceeded {}", item);
    }
    inline auto expected(std::convertible_to<std::string_view> auto item) {
        return std::format("expected {}", item);
    }
    inline auto expected_after(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} after {}", item, context);
    }
    inline auto expected_at(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} at {}", item, context);
    }
    inline auto expected_before(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} before {}", item, context);
    }
    inline auto expected_between(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} between {}", item, context);
    }
    inline auto expected_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} in {}", item, context);
    }
    inline auto expected_of(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("expected {} of {}", item, context);
    }

    inline auto invalid(std::convertible_to<std::string_view> auto item) {
        return std::format("invalid {}", item);
    }
    inline auto invalid_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("invalid {} in {}", item, context);
    }

    inline auto mismatched(std::convertible_to<std::string_view> auto item) {
        return std::format("mismatched {}", item);
    }
    inline auto missing_after(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("missing {} after {}", item, context);
    }
    inline auto mixed(std::convertible_to<std::string_view> auto item) {
        return std::format("mixed {}", item);
    }
    inline auto multiple_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("multiple {} in {}", item, context);
    }

    inline auto unexpected(std::convertible_to<std::string_view> auto item) {
        return std::format("unexpected {}", item);
    }
    inline auto unexpected_after(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("unexpected {} after {}", item, context);
    }
    inline auto unexpected_in(
        std::convertible_to<std::string_view> auto item,
        std::convertible_to<std::string_view> auto context
    ) {
        return std::format("unexpected {} in {}", item, context);
    }
    inline auto unknown(std::convertible_to<std::string_view> auto item) {
        return std::format("unknown {}", item);
    }
    inline auto unterminated(std::convertible_to<std::string_view> auto item) {
        return std::format("unterminated {}", item);
    }
}  // namespace musi::errors
