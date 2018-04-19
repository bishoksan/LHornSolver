#!/bin/sh


START=$(date +%s000)
for file in $1/*
do
  gtimeout 1.0m sh oracle.sh $file
done
END=$(date +%s000)
DIFF=$(( $END - $START ))
echo "total time: $DIFF" >>/Users/jpg/Desktop/results-safe-oracle.txt

rm  widenpoints wut.props traceterm.out
