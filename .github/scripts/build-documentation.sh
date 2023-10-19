#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=channel:nixos-23.05 -i bash --packages coreutils mdbook mdbook-admonish python3 virtualenv ghc stack

set -ex

OUTPUT_DIR="_site"

pushd docs
mdbook-admonish install
popd

python3 <<'EOF' > docs/src/index.md
import sys

with open("README.markdown") as f:
    mode = "title"
    for line in f:
        line = line.rstrip()
        if mode == "title":
            print("Getting Started")
            mode = "after-title"
        elif mode == "after-title":
            if line.startswith("- "):
                mode = "skip-links"
            else:
                print(line)
        elif mode == "skip-links":
            if line.startswith("- "):
                continue
            else:
                mode = "pre-version-table"
                print(line)
        elif mode == "pre-version-table":
            print(line)
            if line.startswith("|"):
                mode = "version-table"
        elif mode == "version-table":
            print(line)
            if line.startswith("See [the latest package documentation]"):
                mode = "after-version-table"
        elif mode == "after-version-table":
            if line.startswith("["):
                mode = "pre-contributing"
                print("")
                print(line)
        elif mode == "pre-contributing":
            if line == "Contributing":
                mode = "skip-to-bibliography"
                continue
            print(line)
        elif mode == "skip-to-bibliography":
            if line == "Bibliography":
                mode = "rest"
                print(line)
        else:
            print(line)

if mode != "rest":
    print(f"unexpected mode: {mode}", file=sys.stderr)
    sys.exit(1)
EOF

bash <<'EOF'
virtualenv venv
source venv/bin/activate
pip install "rst-to-myst"

mkdir -p docs/src/release-notes
for package in concurrency dejafu hunit-dejafu tasty-dejafu; do
  rst2myst convert --no-sphinx "${package}/CHANGELOG.rst"
  cat "${package}/CHANGELOG.md" | \
    sed 'sZ{issue}`\([^`]*\)`Z[issue #\1](https://github.com/barrucadu/dejafu/issues/\1)Zg' | \
    sed 'sZ{pull}`\([^`]*\)`Z[pull request #\1](https://github.com/barrucadu/dejafu/pull/\1)Zg' | \
    sed 'sZ{tag}`\([^`]*\)`Z[\1](https://github.com/barrucadu/dejafu/releases/tag/\1)Zg' | \
    sed 'sZ{u}`\([^`]*\)`Z[\1](https://github.com/\1)Zg' | \
    sed 'sZ{hackage}`\([^`]*\)`Z[\1](https://hackage.haskell.org/package/\1)Zg' > "docs/src/release-notes/${package}.md"
  rm "${package}/CHANGELOG.md"
done

rm -rf venv
EOF

mdbook build docs
mv docs/book "$OUTPUT_DIR"

stack --no-install-ghc --no-nix --skip-ghc-check --system-ghc haddock concurrency dejafu hunit-dejafu tasty-dejafu
rm -rf .stack-work/install/*/*/*/doc/all/
mv .stack-work/install/*/*/*/doc/ "$OUTPUT_DIR/packages"

chmod -c -R +rX "$OUTPUT_DIR" | while read -r line; do
    echo "::warning title=Invalid file permissions automatically fixed::$line"
done
