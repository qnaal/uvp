(flet ((evaluate-deriv (everyone initial-state acc-pol-lst t0 dt &optional in-deriv)
	 "returns a plist of derivatives in the form
 (guysym ((dpx dpy) (dvx dvy)) ...)"
	 (let ((state-lst)
	       (dpos-lst))

	   ;; generate statex from state0 and dt
	   (dotimes (guy-ndx (length everyone))
	     (destructuring-bind (p0 v0)
		 (nth guy-ndx initial-state)
	       (destructuring-bind (dp dv)
		   (or (nth guy-ndx in-deriv)
		       (list (make-pt) (make-pt)))
		 (let ((p1 (v+ p0 (v* dt dp)))
		       (v1 (v+ v0 (v* dt dv))))
		   (setf state-lst (append state-lst
					   (list (list p1 v1)))
			 dpos-lst (append dpos-lst
					  (list v1)))))))

	   ;; acceleration thinks statex is state1, as it well should
	   (let ((dvel-lst (acceleration everyone state-lst acc-pol-lst (+ t0 dt)))
		 (out-deriv))
	     (dotimes (ndx (length everyone) out-deriv)
	       (setf out-deriv (append out-deriv (list (list (nth ndx dpos-lst)
							     (nth ndx dvel-lst))))))))))
  
  (defun integrate (everyone state0-lst acc0-pol-lst t0 dt)
    "rk4-integrates the states from t0 to t0+dt"
    (let* ((a (evaluate-deriv everyone state0-lst acc0-pol-lst t0              0))
	   (b (evaluate-deriv everyone state0-lst acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv everyone state0-lst acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv everyone state0-lst acc0-pol-lst (+ t0 dt)       dt       c))
	   (output))
      (dotimes (ndx (length everyone) output)
	(destructuring-bind ((dp1 dv1);a
			     (dp2 dv2);b
			     (dp3 dv3);c
			     (dp4 dv4);d
			     (pos vel))
	    (list (pop a) (pop b) (pop c) (pop d)
		  (nth ndx state0-lst))
	  (let ((dp (v* 1/6 (v+ dp1 (v* 2 (v+ dp2 dp3)) dp4)))
		(dv (v* 1/6 (v+ dv1 (v* 2 (v+ dv2 dv3)) dv4))))
	    (setf output (append output (list (list (v+ pos (v* dt dp))
						    (v+ vel (v* dt dv))
						    (polarize dv)
						    ))))))))))

;; (defun integrate-euler (everyone state0-lst acc0-pol-lst t0 dt)
;;   (let ((dvel-lst (acceleration everyone state0-lst acc0-pol-lst (+ t0 dt)))
;; 	(output))
;;     (dotimes (ndx (length everyone) output)
;;       (destructuring-bind (dvel
;; 			   (pos0 vel0))
;; 	  (list (pop dvel-lst)
;; 		(nth ndx state0-lst))
;; 	(let ((dpos (v+ vel0 (v* dt dvel))))
;; 	  (setf output (append output (list (list (v+ pos0 (v* dt dpos))
;; 						  (v+ vel0 (v* dt dvel))
;; 						  (polarize dvel)
;; 						  )))))))))
