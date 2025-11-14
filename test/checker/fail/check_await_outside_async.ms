val bar := async proc () { 42 };

val foo := proc () { await bar() };
