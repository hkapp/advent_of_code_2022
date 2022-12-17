#!/bin/bash

if [ ! -d bin ] ; then
  mkdir bin
fi

cd src
ghc -O2 -outputdir ../bin -o ../bin/Main Main.hs
compsta=$?

if [ $compsta -ne 0 ]; then
  exit $compsta
fi

cd ../bin
./Main $1
