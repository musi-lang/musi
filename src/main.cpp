#include <filesystem>
#include <fstream>
#include <print>

#include "lexer.hpp"
#include "parser.hpp"

namespace {
    auto read_file(std::string_view path) -> musi::Result<std::string, std::string> {
        std::ifstream file((std::string(path)));
        if (!file) {
            return std::unexpected(std::format("cannot open file '{}'", path));
        }
        return std::string(std::istreambuf_iterator<char>(file), {});
    }
}  // namespace

auto main() -> int32_t {
    const auto path =
        std::filesystem::current_path().parent_path().parent_path().parent_path().parent_path()
        / "examples" / "test.ms";
    const auto filename = path.filename().string();

    auto content = read_file(path.string());
    if (!content) {
        std::println(stderr, "{}", content.error());
        return 1;
    }

    musi::Source source_file(filename, *content);
    musi::DiagnosticEngine diagnostics(source_file);
    musi::Lexer lexer(source_file, diagnostics);

    auto tokens = lexer.lex();
    if (!tokens || diagnostics.has_errors()) {
        diagnostics.report();
        return 1;
    }
    // for (const auto& token : *tokens) {
    //     std::println("{}", token);
    // }

    musi::Parser parser(*tokens, diagnostics);

    auto nodes = parser.parse();
    if (!nodes || diagnostics.has_errors()) {
        diagnostics.report();
        return 1;
    }
    std::println("{}", *nodes);
}
