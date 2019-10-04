#!/usr/bin/env bash

set -ex

export PATH=$HOME/.local/bin:$PATH

stack="stack --no-terminal"

mkdir -p ~/.local/bin

if [[ -z "$STACKVER" ]]; then
  curl -L https://www.stackage.org/stack/linux-x86_64 | \
    tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
else
  curl -L https://github.com/commercialhaskell/stack/releases/download/v$STACKVER/stack-$STACKVER-linux-x86_64.tar.gz | \
    tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi

if [[ -e ".travis/$RESOLVER.yaml" ]]; then
  mv ".travis/$RESOLVER.yaml" stack.yaml
elif [[ ! -z "$RESOLVER" ]]; then
  $stack init --resolver="$RESOLVER" --force
fi

$stack setup
