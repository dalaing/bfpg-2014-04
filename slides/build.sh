#!/bin/bash
cabal install pandoc
git clone https://github.com/hakimel/reveal.js
pushd reveal.js
git checkout 9da952fea30906090446d038430186b11dba7f13
patch -p1 < ../reveal.patch
popd
pandoc -t revealjs -s free.md -o free.html
