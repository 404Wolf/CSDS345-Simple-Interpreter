#!/usr/bin/env bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

passed=0
failed=0
total=0

# Run through all test cases
for i in {1..28}; do
    num=$(printf "%02d" "$i")
    input="./tests/input/test${num}_in"
    expected="./tests/input/test${num}_out"

    echo -n "Test $num: "

    # Pass the filename to racket through stdin
    output=$(echo "$input" | racket src/runner.rkt)

    # Compare output with expected
    if diff -wB <(echo "$output") "$expected" >/dev/null; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC}"
        echo "Expected:"
        cat "$expected"
        echo ""
        echo "Got:"
        echo "$output"
        echo
        ((failed++))
    fi
    ((total++))
done

# Print summary
echo
echo "Summary:"
echo -e "${GREEN}Passed: $passed${NC}"
echo -e "${RED}Failed: $failed${NC}"
echo "Total:  $total"

# Exit with failure if any tests failed
[ $failed -eq 0 ]
