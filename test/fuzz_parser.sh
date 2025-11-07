#!/bin/bash

TIMEOUT=1
COMPILER="../_build/default/bin/main.exe"

test_cases=(
    "_"
    "_ _"
    "@ @"
    "? ?"
    "! !"
    "{ }"
    "{ _ }"
    "( )"
    "( _ )"
    "[ ]"
    "[ _ ]"
    "const"
    "var"
    "proc"
    "if"
    "while"
    "for"
    "match"
    "."
    ".."
    ":"
    "::"
    ";"
    ";;"
    'import { 123 } from "test";'
    'export { 123 };'
    'proc (123) -> Int;'
)

echo "checking for parser inf loop(s)..."
failed=0

for test in "${test_cases[@]}"; do
    echo -n "Testing: '$test' ... "
    echo "$test" > /tmp/fuzz_test.ms
    timeout $TIMEOUT $COMPILER compile /tmp/fuzz_test.ms > /dev/null 2>&1
    exit_code=$?

    if [ $exit_code -eq 124 ]; then
        echo "TIMEOUT (inf loop?)"
        failed=$((failed + 1))
    else
        echo "OK"
    fi
done

rm -f /tmp/fuzz_test.ms /tmp/fuzz_test.msc

if [ $failed -gt 0 ]; then
    echo "found $failed potential inf loop(s)"
    exit 1
else
    echo "WOHOOO! no inf loop(s) found"
    exit 0
fi
