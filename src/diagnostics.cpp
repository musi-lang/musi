#include "diagnostics.hpp"

#include <filesystem>
#include <sstream>

namespace musi {
    namespace {
        auto format_header(
            std::ostringstream& oss,
            const Source& source,
            const SourceLocation& location
        ) -> void {
            if (source.is_physical_file()) {
                const auto source_path = std::filesystem::path(source.name());
                const auto relative_path = std::filesystem::relative(source_path).string();

                oss << term::OSC << "8;;" << relative_path << term::ST << term::BLUE
                    << term::UNDERLINE << source_path.filename().string() << ":" << location.line()
                    << ":" << location.column() << term::OSC << "8;;" << term::ST << term::RESET;
            } else {
                oss << source.name() << ":" << location.line() << ":" << location.column();
            }
        }
        auto format_severity(std::ostringstream& oss, DiagnosticSeverity severity) -> void {
            switch (severity) {
                case DiagnosticSeverity::None:
                    oss << term::RESET << term::BOLD;
                    break;
                case DiagnosticSeverity::Error:
                    oss << term::RED << term::BOLD << "error: " << term::RESET << term::BOLD;
                    break;
                case DiagnosticSeverity::Warning:
                    oss << term::YELLOW << term::BOLD << "warning: " << term::RESET << term::BOLD;
                    break;
            }
        }
        auto format_source_line(
            std::ostringstream& output,
            const Source& source,
            const SourceLocation& start
        ) -> void {
            const auto content = source.content();
            uint32_t line_start = 0;
            uint32_t line_count = 1;

            for (uint32_t i = 0; i < content.length() && line_count < start.line(); ++i) {
                if (content.at(i) == '\n') {
                    line_start = i + 1;
                    ++line_count;
                }
            }

            auto line_end = line_start;
            while (line_end < content.length() && content.at(line_end) != '\n') {
                ++line_end;
            }

            auto line_content = content.substr(line_start, line_end - line_start);
            if (line_content.empty()) {
                line_content = "\n";
            } else if (!line_content.empty()) {
                output << line_content;
                if (line_content.back() != '\n') {
                    output << '\n';
                }
            } else {
                output << '\n';
            }
        }
        auto format_underline(
            std::ostringstream& oss,
            DiagnosticSeverity severity,
            std::pair<uint32_t, uint32_t> column_range,
            uint32_t content_length
        ) -> void {
            oss << (severity == DiagnosticSeverity::Error ? term::RED : term::YELLOW);

            const auto visual_start = column_range.first;
            const auto visual_end = column_range.second;
            const auto underline_length =
                std::min(visual_end - visual_start, content_length - visual_start + 1);

            oss << std::string(visual_start - 1, ' ');

            if (underline_length > 1) {
                oss << term::BOLD << '^' << std::string(underline_length - 1, '~') << term::RESET;
            } else {
                oss << term::BOLD << '^' << term::RESET;
            }
        }
    }  // namespace

    auto Diagnostic::format(const Source& source) const -> std::string {
        auto oss = std::ostringstream {};

        const auto& start = m_span.start();
        const auto end_column = start.column() + m_span.length();

        format_header(oss, source, start);
        oss << ": ";
        format_severity(oss, m_severity);
        oss << m_message << term::RESET << '\n';
        format_source_line(oss, source, start);
        format_underline(
            oss,
            m_severity,
            { start.column(), end_column },
            static_cast<uint32_t>(source.content().length())
        );
        oss << '\n';
        return oss.str();
    }
}  // namespace musi
