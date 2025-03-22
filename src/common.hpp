#pragma once

#include <algorithm>
#include <cctype>
#include <expected>
#include <functional>
#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace musi {
    /* vendors */
    using Json = nlohmann::ordered_json;

    /* smart pointers */
    template<typename T>
    using Box = std::unique_ptr<T>;

    template<typename T>
    using Ref = std::reference_wrapper<T>;

    /* containers */
    template<typename T>
    using Vec = std::vector<T>;

    template<typename K, typename V>
    using Map = std::unordered_map<K, V>;

    /* monads */
    template<typename T, typename E>
    using Result = std::expected<T, E>;

    template<typename T>
    using Option = std::optional<T>;

    inline auto is_alpha(char ch) -> bool {
        return std::isalpha(ch) != 0;
    }
    inline auto is_alnum(char ch) -> bool {
        return std::isalnum(ch) != 0;
    }
    inline auto is_digit(char ch) -> bool {
        return std::isdigit(ch) != 0;
    }
    inline auto is_xdigit(char ch) -> bool {
        return std::isxdigit(ch) != 0;
    }
    inline auto is_sync_point(char ch) -> bool {
        return ch == '\n' || ch == '\r' || ch == ';' || ch == '}' || ch == ')' || ch == ']'
               || ch == ':' || ch == '=' || ch == '|';
    }

    inline auto to_upper_str(std::string_view str) -> std::string {
        std::string result(str);
        std::ranges::transform(result, result.begin(), [](unsigned char ch) {
            return std::toupper(ch);
        });
        return result;
    }
    inline auto to_lower_str(std::string_view str) -> std::string {
        std::string result(str);
        std::ranges::transform(result, result.begin(), [](unsigned char ch) {
            return std::tolower(ch);
        });
        return result;
    }
}  // namespace musi
