const bar := async proc () { 42 };

const foo := async proc () { await bar() };
