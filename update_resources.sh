#!/usr/bin/env bash

FILES=start.png

for f in $FILES; do
    echo "Copying $f..."
    cp spacer/$f resources/
done
