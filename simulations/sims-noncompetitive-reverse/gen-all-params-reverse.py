#!/usr/bin/env python
## ---------------------------------------------------------------- ##
## GEN-ALL-PARAMS-REVERSE
## ---------------------------------------------------------------- ##
## This file generates a set of Lisp files, each of which runs a
## subset of the simulations that cover the entire parameter space
## of :ALPHA, :EGS, R+ and R- (over 3,000,000 simulations).
## On multiprocessor architectures, each lisp file can be run as a
## single thread, saving significant time.
## ------------------------------------------------------------------ ## 

TMPLT = """
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../pss-device.lisp")
(load "../pss-simulations.lisp")
(defparameter *params* '((:alpha . %0.2f) (:EGS . %0.2f)))
(with-open-file (out "sims-positive-negative-alpha_%0.2f-egs_%0.2f-noncomp-reverse.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-positive-negative-feedback "../pss-model-noncompetitive-reverse.lisp"
				     250
				     (seq 0.0 1.01 1/10)
				     :neg-vals (seq 0.0 -1.01 -1/10)
				     :stream out
				     :report nil
				     :utilities t
				     :params *params*))
"""



#blocks = ((0, 0.5), (0.5, 1.0), (1.0, 1.5), (1.5, 2.0))

   
if __name__ == "__main__":
    i = 1
    for alpha in [x/100.0 for x in range(11)]:
        for egs in [x/10.0 for x in range(11)]:
            fout = open("sims-alpha_%0.2f-egs_%0.2f-noncomp-reverse.lisp" % (alpha, egs), 'w')
            s = TMPLT % (alpha, egs, alpha, egs)
            fout.write(s)
            fout.flush()
            fout.close()
            i = i + 1

    
