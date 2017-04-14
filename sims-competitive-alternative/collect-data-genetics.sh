#!/bin/bash

if [ -f final.txt ]; then
    rm final.txt
fi

if [ -f header ]; then
    rm header
fi

if [ -f final ]; then
    rm header
fi

for i in sims*0.018*.txt; do 
    if [ ! -f header ]; then
	head -1 $i > header
    fi
    echo -n "Collecting $i... "
    cat $i >> final 
    echo "Done"
done

cat header > final-genetics.txt
rm header
grep -v "Avoid" final >> final-genetics.txt
rm final
