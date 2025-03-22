#pragma once

#include <print>
#include <utility>

#include "location.hpp"
#include "source.hpp"

namespace musi {
    namespace term {
        static constexpr std::string_view RESET = "\033[0m";
        static constexpr std::string_view BOLD = "\033[1m";
        static constexpr std::string_view UNDERLINE = "\033[4m";
        static constexpr std::string_view RED = "\033[31m";
        static constexpr std::string_view YELLOW = "\033[33m";
        static constexpr std::string_view BLUE = "\033[34m";
        static constexpr std::string_view CYAN = "\033[36m";

        static constexpr std::string_view OSC = "\x1b]";
        static constexpr std::string_view ST = "\x1b\\";
    }  // namespace term

    enum class DiagnosticSeverity : uint8_t {
        None,
        Error,
        Warning,
    };

    class Diagnostic {
    public:
        Diagnostic(DiagnosticSeverity severity, std::string message, SourceSpan span)
            : m_severity(severity)
            , m_message(std::move(message))
            , m_span(span) {}
        ~Diagnostic() = default;
        Diagnostic(Diagnostic&&) = default;
        Diagnostic(const Diagnostic&) = delete;
        auto operator=(Diagnostic&&) -> Diagnostic& = default;
        auto operator=(const Diagnostic&) -> Diagnostic& = delete;

        [[nodiscard]] constexpr auto severity() const -> DiagnosticSeverity {
            return m_severity;
        }
        [[nodiscard]] constexpr auto message() const -> std::string_view {
            return m_message;
        }
        [[nodiscard]] constexpr auto span() const -> const SourceSpan& {
            return m_span;
        }

        [[nodiscard]] auto format(const Source& source) const -> std::string;

    private:
        DiagnosticSeverity m_severity;
        std::string m_message;
        SourceSpan m_span;
    };

    class DiagnosticEngine {
    public:
        explicit DiagnosticEngine(const Source& source) : m_source(source) {
            m_diagnostics.reserve(INITIAL_DIAGNOSTIC_CAPACITY);
        }

        [[nodiscard]] auto has_errors() const -> bool {
            return std::ranges::any_of(m_diagnostics, [](const auto& diagnostic) {
                return diagnostic.severity() == DiagnosticSeverity::Error;
            });
        }
        [[nodiscard]] auto has_warnings() const -> bool {
            return std::ranges::any_of(m_diagnostics, [](const auto& diagnostic) {
                return diagnostic.severity() == DiagnosticSeverity::Warning;
            });
        }

        auto emit_error(const SourceSpan& span, std::string message) -> void {
            emit(DiagnosticSeverity::Error, span, std::move(message));
        }
        auto emit_warning(const SourceSpan& span, std::string message) -> void {
            emit(DiagnosticSeverity::Warning, span, std::move(message));
        }

        auto report() const -> void {
            for (const auto& diagnostic : m_diagnostics) {
                std::println(stderr, "{}", diagnostic.format(m_source.get()));
            }
        }

    private:
        static constexpr uint8_t INITIAL_DIAGNOSTIC_CAPACITY = 32;

        auto emit(DiagnosticSeverity severity, const SourceSpan& span, std::string message)
            -> void {
            m_diagnostics.emplace_back(severity, std::move(message), span);
        }

        Vec<Diagnostic> m_diagnostics;
        Ref<const Source> m_source;
    };
};  // namespace musi
