#!/bin/bash

for script in sims*0.013*.lisp; do
    nice sbcl --load ${script} &
done
