#!/bin/sh

resultdir="$2"
chc2logen "$1" "$1.logen"
cat "$1.logen" dimsolve.pl.ann > dimsolveProg.pl.ann
f=`basename $1`
echo $f
logengoal="go"

echo $logengoal

# logen
cogen --logen_dir ~/local/logen -np -m dimsolveProg.pl $logengoal > "$resultdir/$f-instr.pl"


#mv "$1-instr.pl" "$resultdir"

rm "$1.logen"
rm dimsolveProg.pl.ann
rm dimsolveProg.pl.gx*




