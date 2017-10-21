pipeline {
  agent any
  stages {
    stage('build') {
      steps {
        sh 'stack --nix build'
      }
    }
    stage('docs') {
      steps {
        sh '''WEB_DIR=/srv/http/docs

function hsdoc {
  srcdir=$(cabal-info hs-source-dirs)
  hsfiles=$(cabal-info exposed-modules | sed -e "s:\\.:/:g" -e "s:$:.hs:")
  otherfiles=$(cabal-info other-modules | sed -e "s:\\.:/:g" -e "s:$:.hs:")

  name=$(cabal-info name)
  version=$(cabal-info version)
  synopsis=$(cabal-info synopsis)

  dir=$1
  url=$2

  tmpdir=`mktemp -d`

  # Highlight source

  mkdir -p $dir/source

  wget "https://raw.githubusercontent.com/houshuang/hackage-css/master/generated_css/tango.css" -O "$dir/source/hscolour.css"

  for file in $hsfiles; do
    HsColour "$srcdir/$file" -css -anchor \\
      -o"$dir/source/`echo $file | sed "s:^\\(.*\\)\\.hs$:\\1:" | sed "s:/:-:g"`.html"
  done

  # Generate documentation

  cabal-info description > "$tmpdir/description"

  macros=$(find . -type f -wholename "./.stack-work/dist/*/autogen/cabal_macros.h")

  find . -type f -wholename "./.stack-work/dist/*/autogen/*.hs" -exec cp "{}" . \\;

  for file in $hsfiles; do
    mkdir -p "$tmpdir/`dirname $file`"
    efile="$tmpdir/`dirname $file`/`basename $file`"
    cpphs "$srcdir/$file" -O"$efile" \\
      --include="$macros" \\
      --strip --strip-eol
  done

  for file in $otherfiles; do
    mkdir -p "$tmpdir/`dirname $file`"
    efile="$tmpdir/`dirname $file`/`basename $file`"
    cpphs "$srcdir/$file" -O"$efile" \\
      --include="$macros" \\
      --strip --strip-eol
  done

  processedfiles=$(find "$tmpdir" -type f -name "*.hs")
  hidefiles=$(cabal-info other-modules | sed "s:^:--hide=:" | tr "\\n" " ")

  stack --nix exec -- \\
    haddock $processedfiles -o "$dir" \\
      --title="$name-$version: $synopsis" \\
      --prologue="$tmpdir/description" \\
      --html --built-in-themes \\
      --source-module="$url/source/%{MODULE/./-}.html" \\
      --source-entity="$url/source/%{MODULE/./-}.html#%N" \\
      --source-entity-line="$url/source/%{MODULE/./-}.html#line-%L" \\
      $hidefiles

  # Clean up
  rm -rf $tmpdir
}

for project in concurrency dejafu hunit-dejafu tasty-dejafu; do
  pushd $project

  docdir="$WEB_DIR/$project"
  docweb="https://docs.barrucadu.co.uk/$project"

  if [[ $BRANCH_NAME == "master" ]]; then
    hsdoc $docdir $docweb
    ln -sf $docdir "$docdir/master"
  else
    hsdoc "$docdir/$BRANCH_NAME" "$docweb/$BRANCH_NAME"
  fi

  popd
done

chmod -R 755 $WEB_DIR'''
      }
    }
  }
}
