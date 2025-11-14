val bar := async proc () { 42 };

val foo := async proc () { await bar() };
