#pragma once

#include <filesystem>

#include "location.hpp"

namespace musi {
    class Source {
    public:
        explicit Source(std::string content)
            : m_name("<source>")
            , m_content(std::move(content))
            , m_physical_file(false) {
            index_lines();
        }
        Source(const std::filesystem::path& path, std::string content)
            : m_name(path.string())
            , m_content(std::move(content))
            , m_physical_file(true) {
            index_lines();
        }

        [[nodiscard]] auto name() const -> std::string_view {
            return m_name;
        }
        [[nodiscard]] auto content() const -> std::string_view {
            return m_content;
        }
        [[nodiscard]] auto is_physical_file() const -> bool {
            return m_physical_file;
        }
        [[nodiscard]] auto location(uint32_t offset) const -> SourceLocation;
        [[nodiscard]] auto line_column(uint32_t offset) const -> LineColumn;
        [[nodiscard]] auto span(uint32_t offset, uint32_t length) const -> SourceSpan;

    private:
        static constexpr uint16_t INITIAL_LINES_CAPACITY = 1024;

        std::string m_name;
        std::string m_content;
        Vec<uint32_t> m_line_offsets;
        bool m_physical_file;

        auto index_lines() -> void;
    };
    class SourceReader {
    public:
        static constexpr auto is_identifier_start(char ch) -> bool {
            return is_alpha(ch) || ch == '_';
        }
        static constexpr auto is_identifier_continue(char ch) -> bool {
            return is_alnum(ch) || ch == '_';
        }

        explicit SourceReader(const Source& source)
            : m_current_location(source.name(), { .line = 1, .column = 1 }, 0)
            , m_source(source) {}

        [[nodiscard]] auto location() const -> const SourceLocation& {
            return m_current_location;
        }
        [[nodiscard]] auto source() const -> const Source& {
            return m_source.get();
        }

        [[nodiscard]] auto is_at_end() const -> bool {
            return m_current_location.offset() == m_source.get().content().length();
        }
        [[nodiscard]] auto peek_at(uint32_t offset) const -> char {
            auto current_offset = m_current_location.offset();
            auto content = m_source.get().content();
            if (current_offset + offset >= content.length()) {
                return '\0';
            }
            return content.at(current_offset + offset);
        }
        [[nodiscard]] auto peek_back(uint32_t offset) const -> char {
            auto current_offset = m_current_location.offset();
            auto content = m_source.get().content();
            if (offset > current_offset) {
                return '\0';
            }
            return content.at(current_offset - offset);
        }
        [[nodiscard]] auto peek() const -> char {
            return peek_at(0);
        }
        [[nodiscard]] auto peek_next() const -> char {
            return peek_at(1);
        }

        auto advance() -> char;
        auto advance_by(uint32_t count) -> void;
        auto advance_until(const std::function<bool()>& predicate) -> void;

    private:
        SourceLocation m_current_location;
        Ref<const Source> m_source;
    };
}  // namespace musi
