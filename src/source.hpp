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
}  // namespace musi
