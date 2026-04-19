extern "C" int clock_ms();

auto native_clock_ms() -> int { return clock_ms(); }
