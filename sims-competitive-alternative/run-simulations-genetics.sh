#!/bin/bash

for script in sims*0.018*.lisp; do
    nice sbcl --load ${script} &
done
