;;; ------------------------------------------------------------------
;;; MODEL SIMULATIONS WITH THE PSS TASK
;;; ------------------------------------------------------------------
;;;
;;; This files contains functions and utilities to simulate the two
;;; PSS Models (canonical and competitive) under different paramater
;;; settings.
;;;
;;; ------------------------------------------------------------------
;;;
;;; (C) 2016, Andrea Stocco,
;;;     University of Washington
;;;     stocco@uw.edu
;;;
;;; ------------------------------------------------------------------



(defun decision-productions ()
  "Returns a sorted list of decision productions for a model, first 'picks' and then 'donts'" 
  (let* ((prods (no-output (spp)))
	 (dos (remove-if-not #'(lambda (x) (string-equal (subseq (symbol-name x) 0 4) "pick")) prods))
	 (donts (remove-if-not #'(lambda (x) (string-equal (subseq (symbol-name x) 0 4) "dont")) prods)))
    (append (sort dos #'string< :key 'symbol-name)
	    (sort donts #'string< :key 'symbol-name))))

(defun decision-utilities (prods)
  "Returns a list of utilities associated with a list of productions"
  (mapcar #'(lambda (x) (caar (no-output (spp-fct (list x :u))))) prods))

;;; ------------------------------------------------------------------
;;; SIMULATION ROUTINES
;;; ------------------------------------------------------------------


(defun simulate (n &key (params nil) (report t) (utilities nil))
  "A generic function to run the model N times. Returns Choose A and Avoid B" 
  (let ((results nil))
    (dotimes (i n)
      (let ((p (make-instance 'pss-task)))
	(suppress-warnings (reload))
	(pss-reload p)
	;(install-device p)
	;(init p)
	;(proc-display)
	(sgp :v nil
	     :style-warnings nil
	     :model-warnings nil)
	
	;; Applies the necessary parameters
	;(when params
	;  (apply 'sgp params))
	(when params
	  (sgp-fct (mapcan #'(lambda (x) (list (first x) (rest x))) params)))

        ;; (sgp :trace-filter production-firing-only)
	(run 3000)

	(let ((partial (calculate-choose-avoid
			(experiment-log (current-device)))))
	  (when utilities
	    (setf partial (append partial
				  (decision-utilities (decision-productions)))))
	  
	  (push partial
		results))))
    (if report
	;(mapcar 'float (list (apply 'mean (mapcar 'first results))
					;		     (apply 'mean (mapcar 'second results))))
	(list (mapcar #'float
		      (eval (let ((qres (mapcar #'(lambda (x) (list 'quote x)) results)))
			      `(mapcar 'mean ,@qres)))))

	(reverse results))))
  


(defun simulate-d2 (n vals)
  "Simulates the effects of different values of D2 (makes sense only for competitive model"
  (let ((results nil))
    (load "pss-model.lisp")   ;; Works only with competitive model
    (dolist (val vals (reverse results))
      (setf *d2* val)
      (let ((partial (simulate n)))
	(format t "~4,f: ~4,f, ~4,f~%" val (first partial) (second partial))
	(push (cons val partial) results)))))

(defparameter *d1d2-column-names* '("D1" "D2" "ChooseA" "AvoidB"
				    "PickA" "PickB" "PickC" "PickD"
				    "PickE" "PickF" "DontPickA" "DontPickB"
				    "DontPickC" "DontPickD" "DontPickE" "DontPickF"))

(defparameter *posneg-column-names* '("PosRwrd" "NegRwrd" "ChooseA" "AvoidB"
				      "PickA" "PickB" "PickC" "PickD"
				      "PickE" "PickF"))

(defparameter *alphaegs-column-names* '("Alpha" "EGS" "ChooseA" "AvoidB"
				      "PickA" "PickB" "PickC" "PickD"
				      "PickE" "PickF"))


(defun simulate-d1-d2 (model n d1-vals &key (d2-vals t) (stream t) (report t) (utilities t) (params nil))
  "Simulates the effects of different values of D1 and D2 (makes sense only for competitive model"
  (load model)   ;; Works only with competitive model
  (format stream "~{~A~^, ~}~%" *d1d2-column-names*)
  (dolist (d1val d1-vals)
    (unless (listp d2-vals)
      (set d2-vals (copy-seq d1-vals)))
    (dolist (d2val d2-vals)
      (setf *d1* d1val)
      (setf *d2* d2val)
      (let ((results (simulate n :report report :utilities utilities :params params)))
	(dolist (partial results)
	  (format stream "~{~5,f~^, ~}~%" (append (list d1val d2val) partial))
	  (finish-output))))))



(defun simulate-negative-feedback (n vals)
  "Simulates the effects of different values of negative reward"
  (let ((results nil))
    (load "pss-model-noncompetitive.lisp")   ;; Works only with non-competitive model
    (dolist (val vals (reverse results))
      (setf *negative-reward* val)
      (let ((partial (simulate n)))
	(format t "~4,f: ~4,f, ~4,f~%" val (first partial) (second partial))
	(push (cons val partial) results)))))

(defun simulate-positive-negative-feedback (model n pos-vals &key (neg-vals t) (stream t) (report t) (utilities t) (params nil))
  "Simulates the effects of different values of positive and negative reward"
  (let ((results nil))
    (load model) 
    (format stream "~{~A~^, ~}~%" *posneg-column-names*)

    (dolist (pval pos-vals (reverse results))
      (unless (listp neg-vals)
	(setf neg-vals (copy-seq pos-vals)))
      (dolist (nval neg-vals)
	(setf *positive-reward* pval)
	(setf *negative-reward* nval)
	(let ((results (simulate n :report report :utilities utilities :params params)))
	(dolist (partial results)
	  (format stream "~{~5,f~^, ~}~%" (append (list pval nval) partial))
	  (finish-output stream)))))))

(defun simulate-alpha-egs (model n alpha-vals &key (egs-vals t) (stream t) (report t) (utilities t))
  "Simulates the effects of different values of ALPHA and EGS"
  (load model)   
  (format stream "~{~A~^, ~}~%" *alphaegs-column-names*)
  (dolist (a-val alpha-vals)
    ;(format t "Anzai!")
    (unless (listp egs-vals)
      (setf egs-vals (copy-seq alpha-vals)))
    ;(format t "Zorro")
    (dolist (e-val egs-vals)
      (setf *d1* 1)
      (setf *d2* 1)
      (setf *positive-reward* 1)
      (setf *negative-reward* -1)
      (let* ((params (list (cons :ALPHA a-val) (cons :EGS e-val)))
	     (results (simulate n :report report :utilities utilities :params params)))
	(dolist (partial results)
	  (format stream "~{~5,f~^, ~}~%" (append (list a-val e-val) partial))
	  (finish-output))))))
