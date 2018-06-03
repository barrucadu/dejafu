#!/usr/bin/env bash

set -ex

export PATH=$HOME/.local/bin:$PATH

stack="stack --no-terminal"

mkdir -p ~/.local/bin

curl -L https://www.stackage.org/stack/linux-x86_64 | \
  tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

if [[ -e ".travis/$RESOLVER.yaml" ]]; then
  mv ".travis/$RESOLVER.yaml" stack.yaml
elif [[ ! -z "$RESOLVER" ]]; then
  $stack init --resolver="$RESOLVER" --force
fi

$stack setup
