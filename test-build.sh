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

# Set the resolver. Uses the environment variable "RESOLVER" if set,
# otherwise whatever is in the "stack.yaml" file.
STACKOPTS="--no-terminal --install-ghc --resolver=$RESOLVER"
if [[ "$RESOLVER" == "" ]]; then
  STACKOPTS="--no-terminal --install-ghc"
fi

# Produce a stack.yaml which doesn't build dejafu (for testing the
# dejafu-0.2 compat of async/hunit/tasty-dejafu)
OLDYAML="stack-old-dejafu.yaml"
sed 's:^- dejafu$::' stack.yaml > $OLDYAML

# Try dejafu-0.2 first
for pkg in hunit-dejafu tasty-dejafu async-dejafu; do
  testcmd "${pkg} (dejafu-0.2)" stack $STACKOPTS --stack-yaml=$OLDYAML test $pkg
done

# Then try dejafu-0.3
testcmd "dpor" stack $STACKOPTS test dpor

echo "== dejafu"
if ! stack $STACKOPTS build dejafu-tests; then
  echo "== FAILED"
  exit 1
fi
if ! stack $STACKOPTS exec dejafu-tests; then
  echo "== FAILED"
  exit 1
fi

for pkg in hunit-dejafu tasty-dejafu async-dejafu; do
  testcmd "${pkg}" stack $STACKOPTS test $pkg
done
