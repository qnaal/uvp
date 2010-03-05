(flet ((evaluate-deriv (everyone state0-indicator t0 dt &optional prev-dstate)
	 "returns a plist of derivatives in the form
 (guysym ((dpx dpy) (dvx dvy)) ...)"
	 ;; (let ((state-lst)
	 ;;       (dpos-lst))

	 ;; generate statex from state0 and dt
	 (dolist (guy everyone)
	   (let* ((state0 (get guy state0-indicator))
		  (p0 (state-pos state0))
		  (v0 (state-vel state0))
		  (dstate (getf prev-dstate guy (make-state)))
		  (dp (state-pos dstate))
		  (dv (state-vel dstate)))
	     (let ((p1 (v+ p0 (v* dt dp)))
		   (v1 (v+ v0 (v* dt dv))))
	       
	       (let ((statex (make-state :pos p1 :vel v1)))
		 (setf (get guy 'statex) statex)
		 ))))

	 ;; acceleration thinks statex is state1, as it well should
	 (let ((dvel-lst (acceleration everyone 'statex (+ t0 dt)))
	       (next-dstate))
	   (dolist (named-dvel dvel-lst)
	     (destructuring-bind (guy dvel) named-dvel ;TODO: make a better structure for this
	       (let* ((state0 (get guy state0-indicator))
		      (dstate (get guy prev-dstate (make-state)))
		      (v0 (state-vel state0))
		      (dv (state-vel dstate))
		      (dpos (v+ v0 (v* dt dv))) ;same as v1 from above
		      )
		 (setf (getf next-dstate guy) ;TODO: using a plist here is slow and useless
		       (make-state :pos dpos
				   :vel dvel)))))
	   next-dstate)))

  (defun integrate (everyone t0 dt)
    "rk4-integrates the states from t0 to t0+dt"
    (let* ((a (evaluate-deriv everyone :state t0              0))
	   (b (evaluate-deriv everyone :state (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv everyone :state (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv everyone :state (+ t0 dt)       dt       c))
	   (output))

      (dolist (guy everyone output)

	(let* (
	       (state0 (get guy :state))
	       (pos (state-pos state0))
	       (vel (state-vel state0))
	       (dstate-a (getf a guy))
	       (dp1 (state-pos dstate-a))
	       (dv1 (state-vel dstate-a))
	       (dstate-b (getf b guy))
	       (dp2 (state-pos dstate-b))
	       (dv2 (state-vel dstate-b))
	       (dstate-c (getf c guy))
	       (dp3 (state-pos dstate-c))
	       (dv3 (state-vel dstate-c))
	       (dstate-d (getf d guy))
	       (dp4 (state-pos dstate-d))
	       (dv4 (state-vel dstate-d)))
	  (let ((dp (v* 1/6 (v+ dp1 (v* 2 (v+ dp2 dp3)) dp4)))
		(dv (v* 1/6 (v+ dv1 (v* 2 (v+ dv2 dv3)) dv4))))
	    (push (list guy 
			(v+ pos (v* dt dp))
			(v+ vel (v* dt dv))
			(polarize dv))
		  output)))))))


(defun integrate (everyone t0 dt)
  (let ((dvel-lst (acceleration everyone :state (+ t0 dt)))
	(output))
    (dolist (named-dvel dvel-lst output)
      (destructuring-bind (guy dvel) named-dvel
	(let* ((state0 (get guy :state))
	       (pos0 (state-pos state0))
	       (vel0 (state-vel state0))
	       (dpos (v+ vel0 (v* dt dvel))))
	  (push (list guy
		      (v+ pos0 (v* dt dpos))
		      (v+ vel0 (v* dt dvel))
		      (polarize dvel))
		output))))))
