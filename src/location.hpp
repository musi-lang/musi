#pragma once

#include <cassert>
#include <format>

#include "common.hpp"

namespace musi {
    struct LineColumn {
    public:
        uint32_t line;
        uint32_t column;

        [[nodiscard]] auto to_json() const -> Json {
            return { { "line", line }, { "column", column } };
        }
    };

    class SourceLocation {
    public:
        SourceLocation() = default;
        SourceLocation(std::string_view filename, LineColumn position, uint32_t offset)
            : m_filename(filename)
            , m_position(position)
            , m_offset(offset) {}

        [[nodiscard]] auto filename() const -> std::string_view {
            return m_filename;
        }
        [[nodiscard]] auto line() const -> uint32_t {
            return m_position.line;
        }
        [[nodiscard]] auto column() const -> uint32_t {
            return m_position.column;
        }
        [[nodiscard]] auto offset() const -> uint32_t {
            return m_offset;
        }

        [[nodiscard]] auto to_json() const -> Json {
            return { { "filename", m_filename },
                     { "position", m_position.to_json() },
                     { "offset", m_offset } };
        }

    private:
        std::string_view m_filename;
        LineColumn m_position {};
        uint32_t m_offset {};
    };

    class SourceSpan {
    public:
        SourceSpan() = default;
        SourceSpan(const SourceLocation& start, size_t length) : m_start(start), m_length(length) {}

        [[nodiscard]] auto start() const -> const SourceLocation& {
            return m_start;
        }
        [[nodiscard]] auto length() const -> size_t {
            return m_length;
        }

        [[nodiscard]] auto to_json() const -> Json {
            return { { "start", m_start.to_json() }, { "length", m_length } };
        }

        static auto merge(const SourceLocation& first, const SourceLocation& second) -> SourceSpan {
            return { first, second.offset() - first.offset() };
        }

    private:
        SourceLocation m_start;
        size_t m_length {};
    };
}  // namespace musi

template<>
struct std::formatter<musi::SourceLocation> {
    constexpr static auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }

    static auto format(const musi::SourceLocation& location, std::format_context& ctx)
        -> std::format_context::iterator {
        return std::format_to(ctx.out(), "{}", location.to_json().dump());
    }
};

template<>
struct std::formatter<musi::SourceSpan> {
    constexpr static auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }

    static auto format(const musi::SourceSpan& span, std::format_context& ctx)
        -> std::format_context::iterator {
        return std::format_to(ctx.out(), "{}", span.to_json().dump());
    }
};
