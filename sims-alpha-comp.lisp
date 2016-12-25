;;; ------------------------------------------------------------------
;;; SIMULATES THE EFFECTS OF ACT-R'S :ALPHA PARAMETER OF THE
;;; BIOLOGICALLY-PLAUSIBLE MODEL
;;; ------------------------------------------------------------------
;;;
;;; This file ran the simulations that lead to identifying the optimal
;;; values of :ALPHA that would best fit the baseline performance
;;; of the control group (in the PD study) or the group averages (in
;;; the genetics study).
;;;
;;; ------------------------------------------------------------------
;;; (c) 2016, Andrea Stocco,
;;;     University of Washington
;;;     stocco@uw.edu
;;; ------------------------------------------------------------------

;; Load ACT-R and the device
(load "/projects/actr/actr7/load-act-r.lisp")
(load "pss-device.lisp")

;; Runs the simulations
(with-open-file (out "sims-alpha-<0,0.1,0.001>-comp.txt" 
		     :direction :output 
		     :if-exists :overwrite 
		     :if-does-not-exist :create)
  (simulate-alpha-egs "pss-model.lisp" 
		      250  
		      (seq 0 0.101 1/1000)
		      :egs-vals '(1/10)
		      :stream out 
		      :report nil 
		      :utilities t))
