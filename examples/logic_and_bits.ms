if x = 5 then x and y               # CXX: if (x == 5) { x & y; }
if x < 5 then x or y                # CXX: if (x < 5) { x | y; }
if x /= 5 then x xor y              # CXX: if (x != 5) { x ^ y; }

x and 5                             # CXX: x & 5
x or 0xFF                           # CXX: x | 0xFF
x xor 42                            # CXX: x ^ 42

if (x and 5) = 3 then y             # CXX: if ((x & 5) == 3) { y; }
if (x or 7) < 10 then y             # CXX: if ((x | 7) < 10) { y; }
if x = (y and 3) then z             # CXX: if (x == (y & 3)) { z; }

if (x and 5) = (y or 3) then z      # CXX: if ((x & 5) == (y | 3)) { z; }
if x < 5 and y > 3 then z           # CXX: if ((x < 5) && (y > 3)) { z; }
if not (x and 5) = 3 then z         # CXX: if (!((x & 5) == 3)) { z; }

if x = 5 and y < 3 then z           # CXX: if ((x == 5) && (y < 3)) { z; }
if x < 5 or y = 3 then z            # CXX: if ((x < 5) || (y == 3)) { z; }
if not x and y = 5 then z           # CXX: if ((!x) && (y == 5)) { z; }

if (x and 5) then y                 # CXX: if (x & 5) { y; }
if x and (y = 5) then z             # CXX: if (x && (y == 5)) { z; }
if (x = 5) and (y < 3) then z       # CXX: if ((x == 5) && (y < 3)) { z; }

(x and 5) + (y or 3)                # CXX: (x & 5) + (y | 3)
if (x + (y and 3)) = 5 then z       # CXX: if ((x + (y & 3)) == 5) { z; }
if x = 5 then y and (z + 1)         # CXX: if (x == 5) { y & (z + 1); }