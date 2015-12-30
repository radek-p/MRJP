#!/bin/bash

dir=$(dirname $0)

pushd ${dir}/../src/
bnfc  --haskell-gadt -p Frontend.Parser Frontend/Parser/Latte.cf
happy -gca Frontend/Parser/ParLatte.y
alex  -g Frontend/Parser/LexLatte.x
for file in Frontend/Parser/*.hs; do
  cat Frontend/Parser/header.pragma ${file} > .tmp;
  mv .tmp ${file};
done
rm Frontend/Parser/*.bak;
popd