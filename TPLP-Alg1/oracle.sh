#!/bin/bash


###########################################################################

HOME_SOURCE="$CIAOPATH/build/bin"
RESULT_FILE="/Users/jpg/Desktop/results-safe-oracle.txt"

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



resultdir=$1_output
f=`basename $1`
echo "File: $f" >>$RESULT_FILE

if (test ! -d $resultdir) then
        mkdir $resultdir
fi



START1=$(date +%s000)

safe $1 $resultdir

END1=$(date +%s000)
DIFF1=$(( $END1 - $START1 ))

echo "total time: $DIFF1" >>$RESULT_FILE
