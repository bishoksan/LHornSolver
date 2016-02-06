#!/bin/bash

# Solves a non-linear Horn clause using a linear solver.#
# Input: a set of Horn clauses#
# Output: safe if the program is solved else unknown if it is not solved#

###########################################################################
#please change direction to HOME_LinearSolve and logFile before running.

HOME_LinearSolve="/Users/kafle/Desktop/LHornSolver/src" # home to all files

logFile="/Users/kafle/Desktop/LHornSolver/res.pl" # stores output of the tool

###########################################################################



# $1 is program name,
# $3 is value of K for the K-dim program,
# $2 is the output file

function verifyCPA(){
    local prog=$1
    local outputFile=$2
    local k=$3
    echo "Computing widening thresholds for specialised program"
    $HOME_LinearSolve/thresholds1 -prg $prog -o wut.props
    echo "Analyse specialised program"
    $HOME_LinearSolve/cpascc -prg $prog  -withwut bounded -wfunc h79 -o $outputFile
    echo "Checking safety"
    # 0 = safe
    # 1 = otherwise (unsafe or unknown)
    $HOME_LinearSolve/checkSafety $outputFile $k
    return $?
}

###########################################################################



# $1  contains the program,
# $2  contains the invariant,


function checkIndInvarint(){
    local prog=$1
    local inv=$2
    # 0 = safe inductive invariant
    # 1 = otherwise
    $HOME_LinearSolve/checkInv -prg  $prog -inv $inv
    return $?
}

###########################################################################


# $1  contains the program,
# $2  has the value of K,
# $3  is the K-dim program,

function generateKdimProgram(){
    local prog=$1
    local k1=$2
    local outputFile=$3
    $HOME_LinearSolve/kdim1 -prg $prog -k $k1  -o $outputFile
}


###########################################################################



# $1  contains the program,
# $2  has the value of K,
# $3  is the K-dim program,

function insertInvariants(){
    local prog=$1
    local inv=$2
    local k=$3
    local outputFile=$4
    $HOME_LinearSolve/insertInvKdim -prg $prog -inv $inv -k $k -o $outputFile
}

###########################################################################


#the main program starts here#

resultdir=$1_output

if (test ! -d $resultdir) then
        mkdir $resultdir
fi

prog1=$1
f=`basename $prog1`
k=0
echo $f >>$logFile

START=$(date +%s000)

#echo "generating $k-dim program" >>$logFile
$HOME_LinearSolve/kdim1 -prg $prog1 -k $k  -o $resultdir/$f.$k.pl
prog=$resultdir/$f.$k.pl
echo "The progr is $prog"

#in shell script 0 is true and any other number is false
while true

do
    verifyCPA $prog $resultdir/$f.pe.cha.pl $k
    ret1=$?
     if [[ $ret1 -eq 1 ]]; then 
            echo "the  program maybe unsolved: unknown" >>$logFile
            break
        else
    #verifyCPA procedure returned safe to a K-dim program and returned a solution, so proceed to check it against the original program

        #echo "checking inductive invariant with $k-dim invariants" >>$logFile
        checkIndInvarint $prog1 $resultdir/$f.pe.cha.pl
        ret2=$?
        if [[ $ret2 -eq 0 ]]; then #is inductive invariant
            echo "the  program is solved" >>$logFile
            #echo "the  inductive inv. are: " >>$logFile
            #           cat $resultdir/$f.pe.cha.pl >>$logFile
            break
        else  #not inductive invariant
              #generate K+1 dim program, insert invariant and go back to repeat
            k1=`expr $k \+ 1`
            echo "generating $k1-dim program"
            generateKdimProgram $prog1  $k1  $resultdir/$f.$k1.pl
            echo "inserting invariants"
            insertInvariants  $resultdir/$f.$k1.pl  "$resultdir/$f.pe.cha.pl"  $k  "$resultdir/$f.$k1-S.pl"
            k=`expr $k \+ 1`
            prog=$resultdir/$f.$k1-S.pl
        fi
    fi
done


END=$(date +%s000)
DIFF=$(( $END - $START ))
echo "total time: $DIFF" >>$logFile
echo DIMENSION = $k >>$logFile

echo "#####################################################################" >>$logFile


