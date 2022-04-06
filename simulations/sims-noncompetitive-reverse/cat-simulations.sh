#!/bin/bash

if [ -e sims-positive-negative-alpha-egs-noncompetitive-reverse.txt ]; then
    rm sims-positive-negative-alpha-egs-noncompetitive-reverse.txt
fi

for f in sims-positive-negative*reverse.txt; do
    echo "Processing $f"
    alpha=`echo $f | cut -f2 -d_ | cut -f1 -d-`
    egs=`echo $f | cut -f3 -d_ | cut -f1 -d-`
    
    if [ ! -e header ]; then
	h=`head -1 $f`
	echo "Alpha, EGS, $h" > header
    fi
    tail -30250 $f > temp
    while read line; do
	echo "$alpha, $egs, $line" >> total
    done < temp
    rm temp
done

cat header total >> sims-positive-negative-alpha-egs-noncompetitive-reverse.txt
rm header total
