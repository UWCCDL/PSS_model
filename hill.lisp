;;; ------------------------------------------------------------------
;;; MULTI-PARAMETER OPTIMIZATION
;;; ------------------------------------------------------------------
;;; Performs derivative-free optimization over parameter space
;;; ------------------------------------------------------------------

(defun ps-point? (pnt)
  "A PS point is a list of keyword/value conses" 
  (and (every 'consp pnt)
       (every 'keywordp (mapcar #'first pnt))))

(defun ps-point-equal? (pnt1 pnt2)
  "Two PS points are equal if they conta
in the same values"
  (and (ps-point? pnt1)
       (ps-point? pnt2)
       (= (length pnt1) (length pnt2))
       (every #'(lambda (x) (member x pnt2 :test #'equalp))
	      pnt1)))

(defun copy-ps-point (pnt)
  "Creates a deep copy of a point"
  (when (ps-point? pnt)
    (mapcar #'(lambda (x) (cons (first x) (rest x)))
	    pnt)))


(defun replace-value (point param newval)
  "Replaces the value of a parameter in a point. Returns a copy"
  (let ((newpoint (copy-ps-point point)))
    (setf (cdr (assoc param newpoint)) newval)
    newpoint))

(defun generate-sampling-points (point param step)
  "Generate sampling points across one dimension (parameter) for a given point"
  (let* ((initial (cdr (assoc param point)))
	 (vals (list (- initial step) initial (+ initial step))))
    (mapcar #'(lambda (x) (replace-value point param x))
	    vals)))

(defun determine-new-space-positions (point steps)
  "Samples across multiple parameters"
  (let ((results (list point)))
    (dolist (pair steps results)
      (let ((param (first pair))
	    (step (rest pair)))
	(setf results
	      (append results (mapcan #'(lambda (x)
					  (generate-sampling-points x param step))
				      results)))))
    (remove-duplicates results :test #'ps-point-equal?)))


(defun parameter-optimize (start steps function reference &optional (previously-sampled nil))
  ;; Calculates an objective function of the results 
  (let* ((samples (determine-new-space-positions start steps))
	 (new-points (remove-if #'(lambda (x) (member
					       x
					       previously-sampled
					       :test #'ps-point-equal?))
				samples))
	 (results (mapcar #'(lambda (point) (simulate 100 point))
			  new-points))
	 (values (mapcar function results))
	 (minimum-value (apply 'min values))
	 (minimum-value-index (position minimum-value values))
	 (min-point (nth minimum-value-index new-points)))
    
    (cond ((< minimum-value reference)
	   (parameter-optimize min-point
			       steps
			       function
			       minimum-value
			       (append previously-sampled new-points)))
	  (t
	   (cons start reference)))))
