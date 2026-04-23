#!/bin/bash


#--------------------------------------------
function test_example() {
    expected=$1
    produced=$2
    
    diff $expected $produced >tmp.diff
    if (test $? == 0); then
	echo "OK"
    else
	echo "Wrong output"
	cat tmp.diff
	echo ""
    fi
    rm -f tmp.diff
}

########### check all 'jp_chkt' examples
echo ""
echo "======================================================="
echo "=== BEGIN tests/test* ================"
for f in ./tests/test_*.hs; do
    echo "****" $(basename "$f") "...." 
    OUTFILE=${f%.hs}".out"
    cabal run < $f > tmp.out 
    test_example "${OUTFILE}" tmp.out
done
echo "=== END tests/test* ================"
echo "======================================================="