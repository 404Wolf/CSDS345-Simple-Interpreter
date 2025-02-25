#!/usr/bin/env bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

passed=0
failed=0
total=0

# Run through all test cases
for i in {1..20}; do
    num=$(printf "%02d" "$i")
    input="./tests/input/test${num}_in"
    expected="./tests/input/test${num}_out"

    echo -n "Test $num: "

    # Check if expected output is "error"
    expected_error=0
    if [ "$(cat "$expected")" = "error" ]; then
        expected_error=1
    fi

    # Pass the filename to racket through stdin and capture both stdout and stderr
    output=$(echo "$input" | racket src/interpreter.rkt 2>&1; echo $?)
    actual_exit_code=${output##*$'\n'}
    output=${output%$'\n'*}

    # Compare output with expected, considering error cases
    if [ $expected_error -eq 1 ]; then
        if [ "$actual_exit_code" -eq 1 ]; then
            echo -e "${GREEN}PASS${NC}"
            ((passed++))
        else
            echo -e "${RED}FAIL${NC}"
            echo "Expected error with exit code 1"
            echo "Got exit code: $actual_exit_code"
            echo "Output: $output"
            echo
            ((failed++))
        fi
    else
        if diff -wB <(echo "$output") "$expected" >/dev/null && [ "$actual_exit_code" -eq 0 ]; then
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

