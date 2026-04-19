[DllImport("clock")]
private static extern int clock_ms();

static int NativeClockMs() => clock_ms();
