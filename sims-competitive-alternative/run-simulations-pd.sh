#!/bin/bash

for script in sims*0.008*.lisp; do
    nice sbcl --load ${script} &
done
