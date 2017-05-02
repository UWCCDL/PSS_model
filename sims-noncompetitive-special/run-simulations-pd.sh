#!/bin/bash

for script in sims*0.003*.lisp; do
    nice sbcl --load ${script} &
done
