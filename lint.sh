#!/usr/bin/env bash

LINT_FAIL_FATAL=true
if hlint --version | grep "^HLint v1" -q; then
  echo "Warning: .hlint.yaml configuration file only supported in HLint v2 and later. NOT considering lint issues an error."
  echo
  LINT_FAIL_FATAL=false
fi

LINT_FAIL=false
for package in concurrency dejafu hunit-dejafu tasty-dejafu dejafu-tests; do
  if ! hlint --no-summary $package; then
    LINT_FAIL=true
  fi
done

if $LINT_FAIL; then
  echo "Lint issues found."
  if $LINT_FAIL_FATAL; then
    exit 1
  fi
fi
