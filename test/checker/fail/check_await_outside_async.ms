const bar := async proc () { 42 };

const foo := proc () { await bar() };
