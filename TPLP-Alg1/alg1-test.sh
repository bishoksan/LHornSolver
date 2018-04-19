#!/bin/sh


echo “analysis started”
START=$(date +%s000)
for file in $1/*
do
gtimeout 1.0m sh alg1.sh $file
done
END=$(date +%s000)
DIFF=$(( $END - $START ))
echo "total time: $DIFF" >>/Users/jpg/Desktop/results.txt

rm  widenpoints wut.props traceterm.out
