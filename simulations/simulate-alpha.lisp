(load "/projects/actr/actr7/load-act-r.lisp")
(load "../pss-device.lisp")
(load "../pss-addition.lisp")
(load "../pss-simulations.lisp")
(setf *d1* 1 *d2* 1)
(with-open-file (out "sims-d1-d2-alpha-0.008-egs-0.1-block16-comp.txt" :direction :output 
		     :if-exists :overwrite :if-does-not-exist :create)
  (simulate-alpha-egs "../pss-model-new-equation.lisp"
		      250
		      (seq 1.50 2.01 1/20)
		      :egs-vals '(0.1)
		      :stream out
		      :report nil
		      :utilities t
		      )) 
(quit)
