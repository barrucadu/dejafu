#!/usr/bin/env bash

function testcmd()
{
  local pkg=$1
  shift
  local stackopts=$*

  echo "== ${pkg}"
  if stack $stackopts test $pkg; then
    echo
  else
    echo "== FAILED"
    exit 1
  fi
}

# Set the resolver. Uses the environment variable "RESOLVER" if set,
# otherwise whatever is in the "stack.yaml" file.
STACKOPTS="--no-terminal --install-ghc --resolver=$RESOLVER"
if [[ "$RESOLVER" == "" ]]; then
  STACKOPTS="--no-terminal --install-ghc"
fi

stack $STACKOPTS setup

# Make sure 'concurrency' builds.
testcmd concurrency $STACKOPTS

# Test 'dejafu'.
echo "== dejafu"
stack $STACKOPS test
