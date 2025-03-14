#include "source.hpp"

#include <algorithm>

namespace musi {
    auto Source::location(uint32_t offset) const -> SourceLocation {
        auto it = std::ranges::upper_bound(m_line_offsets, offset);

        auto line_index = static_cast<uint32_t>(it - m_line_offsets.begin() - 1);
        auto column = offset - m_line_offsets.at(line_index);

        return SourceLocation(
            m_name,
            LineColumn { .line = line_index + 1, .column = column + 1 },
            offset
        );
    }
    auto Source::line_column(uint32_t offset) const -> LineColumn {
        auto current_location = location(offset);
        return LineColumn { .line = current_location.line(), .column = current_location.column() };
    }
    auto Source::span(uint32_t offset, uint32_t length) const -> SourceSpan {
        return { location(offset), length };
    }

    auto Source::index_lines() -> void {
        m_line_offsets.clear();
        m_line_offsets.reserve(INITIAL_LINES_CAPACITY);
        m_line_offsets.push_back(0);

        const auto content = m_content;
        const auto content_length = content.length();

        for (uint32_t i = 0; i < content_length; ++i) {
            if (content.at(i) == '\n') {
                m_line_offsets.push_back(i + 1);
            }
        }
    }
}  // namespace musi
