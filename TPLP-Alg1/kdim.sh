#!/bin/sh
# $1 = file
# $2 = k
# $3 = result directory
# default is upper-bound constraint in position 1
constraint="\\nfalse :- K=$2, false(K,C).\\nfalse :- K=<$2, false(K,C).\\n"
#constraint="\\nfalse :- K=$2, false(K,C).\\n"

propFlags=""
fileSuffix="-atmost-$2.pl"

# read -l flag if present and change to lower-bound constraint
while getopts "l" flag
do
   case $flag in
   l) constraint="\\nfalse :- K>=$3, false(K,C).\\n"
      #constraint="\\nfalse :- K>=$3, false(K,C).\\nfalse :- K=$3, false(K,C).\\n"
      propFlags="-l"
      fileSuffix="-atleast-$3.pl"
      shift
   ;;
   *)
   ;;
   esac
done
#echo $constraint

f=`basename $1`
f=${f%-instr.pl}

filename=$1
resultdir=$3

if [ -f "$filename" ]; then  
  # get the memo facts from the comments in order to rename predicates
  sed -n 's/^\/\* \(memo_table.*\)\*\//\1/p' "$filename" > "$resultdir/$f.memo"
  renamePreds "$filename" "$resultdir/$f.memo" "$resultdir/$f-dim.pl" 


  genProps "$resultdir/$f.memo" "$2" "$resultdir/$f.props" $propFlags
  echo "$constraint" >> "$resultdir/$f-dim.pl"
  pe -prg "$resultdir/$f-dim.pl" -entry false -props "$resultdir/$f.props" -o "$resultdir/$f$fileSuffix"

  #rm "$resultdir/$f-dim.pl"
  #rm "$resultdir/$f.props"
  #rm "$resultdir/$f.memo"

else
   echo "File does not exist : "$filename
fi