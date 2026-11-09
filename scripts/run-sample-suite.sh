#!/bin/bash
# Run math samples, or --working for every sample with a success record.
# Check both process exit and diagnostics; retain complete output per file.
# Usage: scripts/run-sample-suite.sh [--working] [sample/file.egi ...]
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_DIR" || exit 2
TIMINGS="$SCRIPT_DIR/sample-timings.txt"
if [ "${1:-}" = "--working" ]; then
  TIMINGS="$SCRIPT_DIR/working-samples.txt"
  shift
fi
[ -f "$TIMINGS" ] || { echo "ERROR: $TIMINGS not found" >&2; exit 2; }
LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/egison-samples.XXXXXX")" || exit 2

filter_files() {
  if [ "$#" -eq 0 ]; then
    cat "$TIMINGS"
  else
    local file entry
    for file in "$@"; do
      entry="$(awk -v path="$file" '$1 == path { print; found=1; exit } END { if (!found) exit 1 }' "$TIMINGS")"
      if [ -n "$entry" ]; then printf '%s\n' "$entry";
      else printf '%s 30\n' "$file"; fi
    done
  fi
}

fail=0; pass=0
while read -r file expected; do
  case "$file" in '#'*|'') continue ;; esac
  limit=$(( (expected * 3 + 1) / 2 ))
  [ "$limit" -lt 30 ] && limit=30
  log="$LOG_DIR/${file//\//__}.log"
  start=$(date +%s)
  gtimeout -k 5 "$limit" cabal run -v0 egison -- --no-banner --type-check-strict -t "$file" </dev/null >"$log" 2>&1
  rc=$?
  elapsed=$(( $(date +%s) - start ))
  if [ "$rc" -eq 0 ] && ! grep -Eq 'Warning:|Type error:|Parse error|Evaluation error|Assertion failed:|^Error:|^egison:|Exception:' "$log"; then
    status=PASS; pass=$((pass+1))
  else
    status=FAIL; fail=$((fail+1))
  fi
  printf '%-76s %4ds (limit %3ds) %s\n' "$file" "$elapsed" "$limit" "$status"
  if [ "$status" = FAIL ]; then
    printf '  exit=%s; output: %s\n' "$rc" "$log"
    sed -n '1,25p' "$log"
  fi
  # A timeout must not leave a compiler or interpreter running before the
  # next sample. Cabal children inherit the gtimeout process group.
  if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    if ps -axo command | grep -E '(^|/)(cabal(-[0-9.]+)?|ghc(-[0-9.]+)?|runghc|egison)( |$)' | grep -v grep; then
      echo 'ERROR: a Haskell process remains after timeout; stopping.' >&2
      exit 2
    fi
  fi
done < <(filter_files "$@")
printf '\nFINAL: pass=%s fail=%s\nLogs: %s\n' "$pass" "$fail" "$LOG_DIR"
[ "$fail" -eq 0 ]
