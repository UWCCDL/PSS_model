#!/usr/bin/env python
## ---------------------------------------------------------------- ##
## GEN-GENETICS
## ---------------------------------------------------------------- ##
## This file generates a set of Lisp files, each of which runs a
## subset of the simulations that cover the entire parameter space
## of d1 and d2 in the range [0, 2] when the other parameters are
## optimized to reproduce the genetics data (alpha = 0.018, egs = 0.1).
## On multiprocessor machines, each file can be run independently.
## ------------------------------------------------------------------ ## 


TMPLT = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "pss-device.lisp")
(defparameter *params* '((:alpha . 0.018) (:EGS . 0.1)))
(with-open-file (out "sims-d1-d2-alpha-0.018-egs-0.1-block%02d-comp.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-d1-d2 "pss-model.lisp"
                  250
                  (seq %0.2f %0.2f 1/20)
                  :d2-vals (seq %0.2f %0.2f 1/20)
		  :stream out
		  :report nil
		  :utilities t
		  :params *params*))
(quit)
"""

blocks = ((0, 0.5), (0.5, 1.0), (1.0, 1.5), (1.5, 2.0))

   
if __name__ == "__main__":
    i = 1
    for b1 in blocks:
        for b2 in blocks:
            fout = open("sims-d1-d2-alpha-0.018-egs-0.1-block%02d-comp.lisp" % i, 'w')
            s = TMPLT % (i, b1[0], b1[1] + 0.01, b2[0], b2[1] + 0.01)
            fout.write(s)
            fout.flush()
            fout.close()
            i = i + 1

    
