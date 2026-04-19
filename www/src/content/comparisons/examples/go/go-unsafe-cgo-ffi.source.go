// #include "clock.h"
import "C"

func nativeClockMs() int {
    return int(C.clock_ms())
}
