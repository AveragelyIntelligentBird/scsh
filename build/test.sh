#!/bin/sh

srcdir=$1
testmodule=$2

./go -s <<EOF
,config ,load $srcdir/test/test-packages.scm
,open $testmodule
(test-all #t)
,exit
EOF
echo ""

# shouldn't need the last ,exit, but running with it for now