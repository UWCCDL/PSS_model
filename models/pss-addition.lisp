;; Redefines the utility update function.



(defun linear-update-utility (module production reward)
  (let* ((old (production-u production))
	 (alpha (utility-alpha module))
	 (name (symbol-name production))
	 (start (subseq name 0 4)))
    (cond ((string-equal start "PICK")
	   (setf alpha (* *d1* alpha)))
	  ((string-equal start "DONT")
	   (setf alpha (* *d2* alpha)))
	  (t
	   nil))
    (setf (production-u production) (+ old (* alpha (- reward old))))))
