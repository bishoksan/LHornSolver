#!/bin/bash


###########################################################################

HOME_SOURCE="$CIAOPATH/build/bin"
RESULT_FILE="/Users/jpg/Desktop/results.txt"

###########################################################################



###########################################################################


# $1 is program name,
# $2 is directory where intermediate files remain

function safe(){
    local prog=$1
    local f=`basename $prog1`
    local resultdir=$2
    echo "Performing query transformation"
    $HOME_SOURCE/qa $prog -query false -ans -o $resultdir/$f.qa.pl
    echo "Computing widening thresholds"
    $HOME_SOURCE/thresholds1 -prg $resultdir/$f.qa.pl -o wut.props
    echo "Computing convex polyhedron approximation"
    $HOME_SOURCE/cpascc -prg $resultdir/$f.qa.pl  -withwut bounded -wfunc h79 -o $resultdir/$f.qa.cha.pl
    echo "Specialise clauses"
    $HOME_SOURCE/insertProps -prg $prog -props $resultdir/$f.qa.cha.pl -o $resultdir/$f.pe.pl
    $HOME_SOURCE/splitVersions -prg $resultdir/$f.pe.pl -o $resultdir/$f.split.pl
    echo "Computing widening thresholds for specialised program"
    $HOME_SOURCE/thresholds1 -prg $resultdir/$f.split.pl -o wut.props
    echo "Analyse specialised program"
	$HOME_SOURCE/cpascc -prg $resultdir/$f.split.pl -cex "traceterm.out" -withwut bounded -wfunc h79 -o $resultdir/$f.pe.cha.pl
    echo "Checking safety"
    echo "Checking feasibility of trace"
    # return status
    # 1 = safe
    # 0 = unsafe (see traceterm.out for error trace)
    # 2 = spurious error
    counterExample1 $resultdir/$f.split.pl
    retval=$? 
    # return the result from counterExample1
    if [[ $retval -eq 0 ]]; then
    	echo "unsafe" >> $RESULT_FILE
	elif [[ $retval -eq 1 ]]; then
    	echo "safe" >> $RESULT_FILE
	elif [[ $retval -eq 2 ]]; then
		echo "unknown" >> $RESULT_FILE
	fi
    return $retval
}

###########################################################################



###########################################################################

#$1 = k
#$2 = source program
#$3 = directory where intermediate file remains

gen_k_dim_programs(){
	local prog1=$1
	local k=$2
    echo "generating atmost-$k-dim program" #>>$RESULT_FILE
	kdim.sh $prog1 $k $resultdir
	k1=`expr $k \+ 1`
	echo "generating atleast-$k1-dim program" #>>$RESULT_FILE
	kdim.sh -l $prog1 $k1 $resultdir
}

###########################################################################

# Initialise variables
resultdir=$1_output
prog1=$1
f=`basename $prog1`

echo "File: $f" >> $RESULT_FILE
START=$(date +%s000)

if (test ! -d $resultdir) then
        mkdir $resultdir
fi

# generate the instrumented clauses
dimsolve.sh $prog1 $resultdir
instrprog="$resultdir/$f-instr.pl"

terminate=0
k=0
until [ $terminate -eq 1 ]
do
	# generate at-most-k and at-least-k+1 clauses
	gen_k_dim_programs $instrprog $k
	# check safety of at-most-k
	prog="$resultdir/$f-atmost-$k.pl"
	echo "checking safety of ≤$k ..." >> $RESULT_FILE
	safe $prog $resultdir
	ret=$?
	if [ $ret -eq 0 -o $ret -eq 2 ]; then
		terminate=1
	else
		k1=`expr $k \+ 1`
		prog="$resultdir/$f-atleast-$k1.pl"
		echo "checking safety of >$k ..." >> $RESULT_FILE
		safe $prog $resultdir
		ret=$?
    	if [ $ret -eq 0 -o $ret -eq 1 ]; then
    		terminate=1
    	else
    		k=`expr $k \+ 1`
		fi
	fi
done

END=$(date +%s000)
DIFF=$(( $END - $START ))
echo "total time: $DIFF" >> $RESULT_FILE
echo DIMENSION = $k >> $RESULT_FILE
echo "====================" >> $RESULT_FILE
echo "" >> $RESULT_FILE