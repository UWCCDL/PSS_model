#!/bin/bash

for script in sims*0.007*.lisp; do
    nice sbcl --load ${script} &
done
