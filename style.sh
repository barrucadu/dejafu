#!/usr/bin/env bash

for package in concurrency dejafu hunit-dejafu tasty-dejafu; do
  find $package -name '*.hs' -exec stylish-haskell -i {} \;
done
