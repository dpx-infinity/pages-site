#!/bin/bash

DIR="$(cd "$(dirname "$0")"; pwd)"
SITEDIR="$DIR/../dpx-infinity.github.com"

VERBOSE=
DO_PUSH=

for arg in "$@"; do
    case "$arg" in
        -v)
            VERBOSE=yes
            ;;
        -p)
            DO_PUSH=yes
            ;;
    esac
done

if [[ ! -d "$SITEDIR" ]]; then
    echo "$SITEDIR does not exist or is not directory!"
    exit 1
fi

echo "Compiling the site."

if [[ -n "$VERBOSE" ]]; then
    runhaskell main.hs rebuild
else
    runhaskell main.hs rebuild >/dev/null
fi

cp -r _site/* "$SITEDIR"

echo "Done compiling."

if [[ "$1" == "-p" ]]; then
    echo "Pushing the site"
    cd "$SITEDIR"
    git add .
    git commit -am "Site update"
    git push
    echo "Done."
fi
