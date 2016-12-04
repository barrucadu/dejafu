#!/usr/bin/env bash

function testcmd()
{
  local name=$1
  shift
  local command=$*

  echo "== ${name}"
  if $command; then
    echo
  else
    echo "== FAILED"
    exit 1
  fi
}

function testdejafu()
{
  local name=$1
  shift
  local stackopts=$*

  echo "== $name"
  if ! stack $stackopts build dejafu-tests; then
    echo "== FAILED (build)"
    exit 1
  fi
  if ! stack $stackopts exec dejafu-tests; then
    echo "== FAILED (test)"
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

testdejafu "dejafu" $STACKOPTS

for pkg in hunit-dejafu tasty-dejafu async-dejafu; do
  testcmd "${pkg}" stack $STACKOPTS test $pkg
done
