auto find_gate(std::string_view ticket) -> std::optional<int>{
    if (ticket == "A12") return 4;
    return std::nullopt;
}
