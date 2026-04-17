#!/usr/bin/env bash
# run_tests.sh - cob2csv test suite
# Tests both cob2csv and csv2cob using diff-based assertions

set -e
PASS=0
FAIL=0
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_DIR="$SCRIPT_DIR/../bin"
TEST_DIR="$SCRIPT_DIR"
TMP_DIR=$(mktemp -d)

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

pass() { echo -e "${GREEN}PASS${NC}: $1"; PASS=$((PASS + 1)); }
fail() { echo -e "${RED}FAIL${NC}: $1"; FAIL=$((FAIL + 1)); }

cleanup() { rm -rf "$TMP_DIR"; }
trap cleanup EXIT

echo "========================================"
echo " cob2csv test suite"
echo "========================================"

# -------------------------------------------------------
# Test 1: cob2csv basic conversion
# -------------------------------------------------------
echo ""
echo "Test 1: Fixed-width -> CSV (customer)"
OUT="$TMP_DIR/customer_out.csv"
"$BIN_DIR/cob2csv" \
    -s "$TEST_DIR/customer.cfg" \
    -i "$TEST_DIR/customer.dat" \
    -o "$OUT" 2>/dev/null

if diff -q "$TEST_DIR/customer_expected.csv" "$OUT" > /dev/null 2>&1; then
    pass "cob2csv customer output matches expected"
else
    fail "cob2csv customer output differs from expected"
    echo "  Expected:"
    cat "$TEST_DIR/customer_expected.csv" | sed 's/^/    /'
    echo "  Got:"
    cat "$OUT" | sed 's/^/    /'
fi

# -------------------------------------------------------
# Test 2: csv2cob roundtrip - CSV back to fixed-width
# -------------------------------------------------------
echo ""
echo "Test 2: CSV -> Fixed-width roundtrip (customer)"
ROUNDTRIP="$TMP_DIR/customer_roundtrip.dat"
"$BIN_DIR/csv2cob" \
    -s "$TEST_DIR/customer.cfg" \
    -i "$TEST_DIR/customer_expected.csv" \
    -o "$ROUNDTRIP" 2>/dev/null

if diff -q "$TEST_DIR/customer.dat" "$ROUNDTRIP" > /dev/null 2>&1; then
    pass "csv2cob roundtrip matches original .dat"
else
    fail "csv2cob roundtrip differs from original .dat"
    echo "  Expected (hex):"
    xxd "$TEST_DIR/customer.dat" | head -5 | sed 's/^/    /'
    echo "  Got (hex):"
    xxd "$ROUNDTRIP" | head -5 | sed 's/^/    /'
fi

# -------------------------------------------------------
# Test 3: cob2csv missing schema arg
# -------------------------------------------------------
echo ""
echo "Test 3: Error handling - missing schema"
if "$BIN_DIR/cob2csv" -i "$TEST_DIR/customer.dat" 2>&1 | \
    grep -q "ERROR"; then
    pass "cob2csv reports error on missing schema"
else
    fail "cob2csv did not report error on missing schema"
fi

# -------------------------------------------------------
# Test 4: cob2csv missing input arg
# -------------------------------------------------------
echo ""
echo "Test 4: Error handling - missing input file"
if "$BIN_DIR/cob2csv" -s "$TEST_DIR/customer.cfg" 2>&1 | \
    grep -q "ERROR"; then
    pass "cob2csv reports error on missing input"
else
    fail "cob2csv did not report error on missing input"
fi

# -------------------------------------------------------
# Summary
# -------------------------------------------------------
echo ""
echo "========================================"
echo " Results: $PASS passed, $FAIL failed"
echo "========================================"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
