#!/bin/bash
# Run all sample/math/* files with per-sample timeout based on
# scripts/sample-timings.txt. Timeout = expected * 1.5 (min 5s).
# FAIL-tagged samples (>180s in timings) get 300s to allow potential
# PASS once their root cause is fixed.
#
# Usage:
#   scripts/run-sample-suite.sh                  # all samples
#   scripts/run-sample-suite.sh sample/foo.egi   # one specific file
#
# Output format:
#   <path>  <wall>s (timeout <to>s) PASS|FAIL
#   ...
#   FINAL: pass=<P> fail=<F>
#   FAILS: <list>

set -u
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TIMINGS="$SCRIPT_DIR/sample-timings.txt"

if [ ! -f "$TIMINGS" ]; then
  echo "ERROR: $TIMINGS not found" >&2
  exit 2
fi

# When called with arguments, only run those specific files.
filter_files() {
  if [ $# -eq 0 ]; then
    cat "$TIMINGS"
  else
    for arg in "$@"; do
      grep "^$arg " "$TIMINGS" || echo "$arg 30"  # default 30s for unknown
    done
  fi
}

fail=0; pass=0; failed=""
while IFS=' ' read -r f expected; do
  case "$f" in '#'*|'') continue ;; esac
  to=$(( (expected * 3 + 1) / 2 ))
  [ "$to" -lt 5 ] && to=5
  start=$(date +%s)
  gtimeout "$to" cabal run -v0 egison -- -t "$f" >/dev/null 2>&1
  rc=$?
  end=$(date +%s)
  dt=$((end - start))
  if [ $rc -eq 0 ]; then
    status="PASS"; pass=$((pass+1))
  else
    status="FAIL"; fail=$((fail+1)); failed="$failed\n$f"
  fi
  printf "%-72s %4ds (timeout %3ds) %s\n" "$f" "$dt" "$to" "$status"
done < <(filter_files "$@")

echo
echo "FINAL: pass=$pass fail=$fail"
[ -n "$failed" ] && echo -e "FAILS:$failed"
