int find_gate(const char *ticket, int *out_gate) {
    if (strcmp(ticket, "A12") == 0) {
        *out_gate = 4;
        return 1;
    }
    return 0;
}
