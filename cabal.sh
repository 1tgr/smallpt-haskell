#!/bin/bash
packageDir=`cd $0/.. ; pwd`/packages
cabal install --prefix=$packageDir
