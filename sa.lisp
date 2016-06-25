;;;
;;; Searches through parameter space
;;;
;;; 3^N -1
;;;


(defparameter *params* '((:alpha 0.1 0 1 0.05)
			 (:egs 0.1 0 1 0.1)))
			 

(defun seq (start finish &optional (step 1))
  (let ((result nil)
	(val start))
    (while (<= val finish)
      (push val result)
      (setf val (+ val step)))
    (reverse result)))

(defun same (x)
  "Returns the same evalue"
  x)

(defun asinsqrt (x)
  "Normalizes accuracy data 0 < x < 1"
  (asin (sqrt x)))
	

(defun energy (results &optional (comparison (rest (assoc 'controls *frank-data*))))
  "Calculates the energy, as the distance from the ideal state"
  (let ((res (mapcar #'(lambda (x) (* 100 x)) results))
	(comp (mapcar #'(lambda (x) (* 100 x)) comparison)))
    (/ (reduce #'+
	       (mapcar #'(lambda (x y) (expt (- x y) 2))
		       res comp))
       100)))


(defun param-list? (struct)
  "A list made of param name, start value, min value, max value, step size"
  (and (= (length struct) 5)
       (every #'numberp (rest struct))
       (keywordp (first struct))))

(defun param-name (lst)
  (first lst))

(defun param-start-value (lst)
  (second lst))

(defun param-min-value (lst)
  (third lst))

(defun param-max-value (lst)
  (fourth lst))

(defun param-step-size (lst)
  (fifth lst))

(defun 1d-point? (lst)
  (and (consp lst)
       (keywordp (first lst))
       (numberp (rest lst))))

(defun 1d-neighborhood (1d-point param-list)
  "Returns the neighbors of a 1D point"
  (let* ((dimension (first 1d-point))
	 (param (assoc dimension param-list))
	 (step (param-step-size param))
	 (v-min (param-min-value param))
	 (v-max (param-max-value param))
	 (current (rest 1d-point))
	 (low (max (- current step) v-min))
	 (high (min (+ current step) v-max)))
    (mapcar #'(lambda (x) (cons dimension x))
	    (list low current high))))

(defun hyperpoint? (lst)
  (and (> (length lst) 1)
       (every #'1d-point? lst)))

(defun create-hyperpoint (param-lists)
  (reverse (pairlis (mapcar #'param-name param-lists)
		    (mapcar #'param-start-value param-lists))))

(defun equal-hyperpoints (hp1 hp2)
  "Two hyperpoits are equal if they have the same values for each coordinate" 
  (let ((p1 (sort hp1 #'string-lessp :key #'first))
	(p2 (sort hp2 #'string-lessp :key #'first)))
    (equalp p1 p2)))


(defun cartesian-product (lst)
  (if (= (length lst) 1)
      (mapcar #'list (first lst))
      (let ((results nil)
	    (a (first lst))
	    (b (cartesian-product (rest lst))))
	(dolist (i a results)
	  (dolist (j b)
	    (push (push i j) results))))))  

(defun hyperneighborhood (hyperpoint param-lists)
  (remove-duplicates
   (cartesian-product (mapcar #'(lambda (x) (1d-neighborhood x param-lists))
			      hyperpoint))))

(defun evaluate-hyperpoint (hyperpoint &optional (n 100))
  (energy (simulate n :params hyperpoint)))

(defun simulated-annealing (param-lists &key (start-temp 10) (end-temp 0.001) (frac 0.5))
  (format t "Starting SA...~%")
  (force-output)
  (let* ((current (create-hyperpoint param-lists))
	 (e0 (evaluate-hyperpoint current))
	 (ctemp start-temp))
    
    (while (> ctemp end-temp)
      (format t "Temp = ~5,f, Energy = ~5,f~%" ctemp e0)
      (force-output)
      (let* ((next (pick (remove current (hyperneighborhood current param-lists))))
	     (e1 (evaluate-hyperpoint next))
	     (delta (- e1 e0)))
	(format t "  New energy = ~2,f~%" e1)
	(cond ((< delta 0)
	       (setf current next)
	       (setf e0 e1))
	      (t
	       (let ((rand (random 0.999999))
		     (metro (exp (/ (* -1 delta) ctemp))))
		 (format t "     Metro: ~6,f~%" metro)
		 (when (< rand metro)
		   (setf current next)
		   (setf e0 e1))))))
      (setf ctemp (* frac ctemp)))
    current))
