#!/bin/bash
# Test Emacs startup time

set -e

THRESHOLD=${1:-1.0}

echo "Testing Emacs startup time (threshold: ${THRESHOLD}s)..."

TIME=$(emacs --batch --eval "
  (let ((start (current-time)))
    (load \"~/.emacs.d/early-init.el\" nil t)
    (load \"~/.emacs.d/init.el\" nil t)
    (princ (format \"%.2f\" (float-time (time-subtract (current-time) start)))))" 2>/dev/null)

echo "Startup time: ${TIME}s"

if [ "$(echo "$TIME < $THRESHOLD" | bc -l)" -eq 1 ]; then
  echo "PASS"
  exit 0
else
  echo "FAIL: exceeds ${THRESHOLD}s threshold"
  exit 1
fi
