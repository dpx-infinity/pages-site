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
    echo "=== $SITEDIR does not exist or is not directory!"
    exit 1
fi

echo "=== Compiling the site."

if [[ -n "$VERBOSE" ]]; then
    runhaskell main.hs rebuil 
else
    runhaskell main.hs rebuild >/dev/null
fi
if [[ "$?" != "0" ]]; then 
    echo "=== Compilation failed" 
    exit 1
fi

cp -r _site/* "$SITEDIR"

echo "=== Done compiling."

if [[ "$DO_PUSH" == "yes" ]]; then
    echo "=== Redeploying the site"
    cd "$SITEDIR"
    echo "=== Adding... "
    git add .
    echo "=== Committing... "
    git commit -am "Site update"
    echo "=== Pushing... "
    git push
    echo "=== Done."
fi
