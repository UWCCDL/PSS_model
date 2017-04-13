#!/bin/bash

for script in sims*.lisp; do
    nice sbcl --load ${script} &
done
