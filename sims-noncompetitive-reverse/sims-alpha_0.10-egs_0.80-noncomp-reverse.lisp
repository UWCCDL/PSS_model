
(load "/projects/actr/actr7/load-act-r.lisp")
(load "../pss-device.lisp")
(load "../pss-simulations.lisp")
(defparameter *params* '((:alpha . 0.10) (:EGS . 0.80)))
(with-open-file (out "sims-positive-negative-alpha_0.10-egs_0.80-noncomp-reverse.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-positive-negative-feedback "../pss-model-noncompetitive-reverse.lisp"
				     250
				     (seq 0.0 1.01 1/10)
				     :neg-vals (seq 0.0 -1.01 -1/10)
				     :stream out
				     :report nil
				     :utilities t
				     :params *params*))
