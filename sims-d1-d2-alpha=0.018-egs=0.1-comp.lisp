;;; ------------------------------------------------------------------
;;; SIMULATES THE EFFECTS OF D1 AND D2 FOR THE PD STUDY  
;;; ------------------------------------------------------------------
;;;
;;; This is a biologically-plausible, competitive implementatin of
;;; of the PSS task in ACT-R. Different than the canonical model,
;;; it requires two setb of productions ("Pick A, B, ... F" and "Don't
;;; Pick A, B,... F"), whose utilities are updated and controlled
;;; by different equations and parameters (see pss-device.lisp). 
;;;
;;; ------------------------------------------------------------------
;;; (c) 2016, Andrea Stocco,
;;;     University of Washington
;;;     stocco@uw.edu
;;; ------------------------------------------------------------------


;; Simulates the effects of D1/D2 when Alpha = 0.008 and EGS = 0.1
;;

;; Load ACT-R and the device
(load "/projects/actr/actr7/load-act-r.lisp")
(load "pss-device.lisp")
(defparameter *params* '((:ALPHA . 0.008) (:EGS . 0.1)))

;; Runs the simulations
(with-open-file (out "sims-d1-d2-alpha-0.008-egs-0.1-comp.txt" 
		     :direction :output 
		     :if-exists :overwrite 
		     :if-does-not-exist :create)
  (simulate-d1-d2 "pss-model.lisp"
		  250
		  (seq 0 2.01 0.1) 
		  :stream out 
		  :report nil
		  :utilities t 
		  :params *params*))

