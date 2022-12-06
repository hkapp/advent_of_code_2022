#!/bin/bash

if [ ! -d bin ] ; then
  mkdir bin
fi

cd src
ghc -outputdir ../bin -o ../bin/Main Main.hs

cd ../bin
./Main $1
