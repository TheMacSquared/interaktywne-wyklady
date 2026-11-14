#!/usr/bin/env bash
# Uruchamia wszystkie kontrole obu kursów; wyniki w $1 (katalog), podsumowanie na stdout.
S="$1"
Rscript statystyka/tests/testthat.R > "$S/tests_stat_after.log" 2>&1
echo "statystyka testthat: exit $? $(grep -E '^\[ FAIL' "$S/tests_stat_after.log" | tail -1)"
Rscript analiza-ryzyka/tests/testthat.R > "$S/tests_ar_after.log" 2>&1
echo "analiza-ryzyka testthat: exit $? $(grep -E '^\[ FAIL' "$S/tests_ar_after.log" | tail -1)"
grep -E "^(Failure|Error) \(" "$S/tests_stat_after.log" "$S/tests_ar_after.log" | sort -u | cut -c1-220
fail=0
for f in statystyka/tests/js/*.test.js analiza-ryzyka/tests/js/*.test.js; do
  if ! node "$f" > "$S/node.log" 2>&1; then
    fail=$((fail + 1)); echo "NODE FAIL $f"; tail -5 "$S/node.log"
  fi
done
echo "node: błędów $fail"
echo "-- kontrakt statystyka"; Rscript statystyka/scripts/check_design_contract.R --strict 2>&1 | tail -7
echo "-- kontrakt analiza-ryzyka"; Rscript analiza-ryzyka/scripts/check_design_contract.R --strict 2>&1 | tail -3
echo "-- zależności"; Rscript statystyka/scripts/check_dependencies.R 2>&1 | tail -2
