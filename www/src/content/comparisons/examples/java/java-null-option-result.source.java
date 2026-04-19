Optional<Integer> findGate(String ticket) {
    return ticket.equals("A12") ? Optional.of(4) : Optional.empty();
}
