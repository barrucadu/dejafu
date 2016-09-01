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

# Test dejafu-0.2 compat of async/hunit/tasty-dejafu
if [[ -z "$SKIP_DEJAFU_02" ]]; then
  sed 's:^- dejafu$::' stack.yaml > stack-dejafu_02.yaml
  sed -i 's/^extra-deps: \[\]/extra-deps: [ dejafu-0.2.0.0 ]/' stack-dejafu_02.yaml

  for pkg in hunit-dejafu tasty-dejafu; do
    testcmd "${pkg} (dejafu-0.2)" stack $STACKOPTS --stack-yaml=stack-dejafu_02.yaml test $pkg
  done
fi

# Test dpor-0.1 compat of dejafu
if [[ -z "$SKIP_DPOR_01" ]]; then
  # Use 0.1.0.1 because it builds with ghc 8.
  sed 's:^- dpor$::' stack.yaml > stack-dpor_01.yaml
  sed -i 's/^extra-deps: \[\]/extra-deps: [ dpor-0.1.0.1 ]/' stack-dpor_01.yaml

  testdejafu "dejafu (dpor-0.1)" $STACKOPTS --stack-yaml=stack-dpor_01.yaml
fi

# Test dpor-0.2 compat of dejafu
if [[ -z "$SKIP_DPOR_02" ]]; then
  sed 's:^- dpor$::' stack.yaml > stack-dpor_02.yaml
  sed -i 's/^extra-deps: \[\]/extra-deps: [ dpor-0.2.0.0 ]/' stack-dpor_02.yaml

  testdejafu "dejafu (dpor-0.2)" $STACKOPTS --stack-yaml=stack-dpor_02.yaml
fi

# Test HEAD version of everything
testcmd "dpor" stack $STACKOPTS test dpor

testdejafu "dejafu" $STACKOPTS

for pkg in hunit-dejafu tasty-dejafu async-dejafu; do
  testcmd "${pkg}" stack $STACKOPTS test $pkg
done
