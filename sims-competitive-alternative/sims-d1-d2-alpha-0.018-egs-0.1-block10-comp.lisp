
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../pss-device.lisp")
(load "../pss-addition.lisp")
(load "../pss-simulations.lisp")
(defparameter *params* '((:alpha . 0.018) (:EGS . 0.1)))
(with-open-file (out "sims-d1-d2-alpha-0.018-egs-0.1-block10-comp.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-d1-d2 "../pss-model-new-equation.lisp"
                  250
                  (seq 1.00 1.51 1/20)
                  :d2-vals (seq 0.50 1.01 1/20)
		  :stream out
		  :report nil
		  :utilities t
		  :params *params*))
(quit)
