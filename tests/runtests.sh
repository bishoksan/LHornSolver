#!/bin/bash

# TODO: Does not have timeouts (do not use gtimeout, do it in Ciao)
# TODO: Needs a value for CIAOPATH

# Exit immediately if a simple command exits with a non-zero status
set -e

# Physical directory where the script is located
_base=$(e=$0;while test -L "$e";do d=$(dirname "$e");e=$(readlink "$e");\
        cd "$d";done;cd "$(dirname "$e")";pwd -P)

set -e

solver=$CIAOPATH/build/bin/lhornsolver

testdir="../../RAHFT/examples" # TODO: Using examples from RAHFT!
tests="\
addition.nts.pl \
bfprt.nts.pl \
binarysearch.nts.pl \
buildheap.nts.pl \
countZero.nts.pl \
fib.pl \
identity.nts.pl \
merge.nts.pl \
palindrome.nts.pl \
parity.nts.pl \
remainder.nts.pl \
running.nts.pl \
triple.nts.pl"
# tests="\
# addition.nts.pl \
# bfprt.nts.pl \
# binarysearch.nts.pl \
# buildheap.nts.pl \
# coins.nts.pl \
# countZero.nts.pl \
# fib.pl \
# identity.nts.pl \
# mc91.pl \
# merge.nts.pl \
# palindrome.nts.pl \
# parity.nts.pl \
# remainder.nts.pl \
# running.nts.pl \
# triple.nts.pl"

results="result.txt"

cd "$_base"

rm -f "$results"
for i in $tests; do
    $solver "$testdir/$i"
done

if diff <(sed 's/, Time:.*/}/g' "$results") \
	<(sed 's/, Time:.*/}/g' "$results"-good); then
    printf "\nTESTS SEEMS OK\n"
else
    printf "\nTESTS DIFFER, SOMETHING MAY BE WRONG\n"
fi
