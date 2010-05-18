(flet ((evaluate-deriv (state0-lst t0 dt &optional prev-dstate) ;perhaps return a hashtable of derivs?
	 "returns the derivatives of the states over dt"
	 (let ((statex-lst))
	   ;; build statex-lst
	   (dolist (state0 state0-lst)	;euler-integrate state0 with prev-dstate to get statex
	     (with-slots ((thing symbol) (p0 pos) safe (v0 vel)) state0
	       (let ((dstate (if prev-dstate
				 (find thing prev-dstate :key #'state-symbol)
				 (make-state :symbol t ;a blank deriv to pretend to do math to
					     :pos (make-pt-board)
					     :vel (make-pt-board)))))
		 (with-slots ((dp pos) (dv vel)) dstate
		   (let ((p1 (v+ p0 (v* dt dp)))
			 (v1 (v+ v0 (v* dt dv))))
		     (let ((statex (make-state :symbol thing :pos p1 :safe safe :vel v1)))
		       (push statex statex-lst)))))))
	   ;; acceleration thinks statex is state1, as it well should
	   (let ((dvel-lst (acceleration statex-lst (+ t0 dt)))
		 (next-dstate))
	     (dolist (state0 state0-lst)
	       (with-slots ((thing symbol) (p0 pos) (v0 vel)) state0
		 (let ((dstate (if prev-dstate
				   (find thing prev-dstate :key #'state-symbol)
				   (make-state :symbol t
					       :pos (make-pt-board)
					       :vel (make-pt-board)))))
		   (with-slots ((dv vel)) dstate
		     (let ((out-dpos (v+ v0 (v* dt dv))) ;same as v1 from above
			   (out-dvel (cdr (assoc thing dvel-lst))))
		       (push (make-state :symbol thing
					 :pos out-dpos
					 :vel out-dvel)
			     next-dstate))))))
	     next-dstate))))

  (defun integrate-rk4 (state0-lst t0 dt)
    "rk4-integrates the states from t0 to t0+dt"
    (let* ((a (evaluate-deriv state0-lst t0              0))
	   (b (evaluate-deriv state0-lst (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv state0-lst (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv state0-lst (+ t0 dt)       dt       c))
	   (output))
      (dolist (state0 state0-lst output)
	(with-slots ((thing symbol) (pos0 pos) safe (vel0 vel)) state0
	  (with-slots ((dp1 pos) (dv1 vel)) (find thing a :key #'state-symbol)
	    (with-slots ((dp2 pos) (dv2 vel)) (find thing b :key #'state-symbol)
	      (with-slots ((dp3 pos) (dv3 vel)) (find thing c :key #'state-symbol)
		(with-slots ((dp4 pos) (dv4 vel)) (find thing d :key #'state-symbol) ;christ above
		  (let ((dp (v* 1/6 (v+ dp1 (v* 2 (v+ dp2 dp3)) dp4)))
			(dv (v* 1/6 (v+ dv1 (v* 2 (v+ dv2 dv3)) dv4))))
		    (push (make-state :symbol thing
				      :pos (v+ pos0 (v* dt dp))
				      :safe (if (safe-check pos0 safe *map*)
						pos0
						(attribute thing :safe))
				      :vel (v+ vel0 (v* dt dv)))
			  output)))))))))))


(defun integrate-euler (state0-lst t0 dt)
  "euler-integrates from state0 to state1"
  (let ((dvel-lst (acceleration state0-lst (+ t0 dt)))
	(state1-lst))
    (dolist (state0 state0-lst state1-lst)
      (with-slots ((thing symbol) (pos0 pos) safe (vel0 vel)) state0
	(let* ((dvel (cdr (assoc thing dvel-lst)))
	       (dpos (v+ vel0 (v* dt dvel))))
	  (push (make-state :symbol thing
			    :pos (v+ pos0 (v* dt dpos))
			    :safe (if (safe-check pos0 safe *map*)
				      pos0
				      (attribute thing :safe))
			    :vel (v+ vel0 (v* dt dvel)))
		state1-lst))))))
