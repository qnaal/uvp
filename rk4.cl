(flet ((evaluate-deriv (everyone initial-state acc-pol-lst t0 dt &optional in-deriv)
	 "returns a plist of derivatives in the form
 (guysym ((dpx dpy) (dvx dvy)) ...)"
	 (let ((state-lst)
	       (dpos-lst))
	   (dotimes (guy-ndx (length everyone))
	     (destructuring-bind ((p0 v0)
				  (dp dv))
		 (list (nth guy-ndx initial-state)
		       (or (nth guy-ndx in-deriv)
			   '((0 0) (0 0))))
	       (let ((p1 (v+ p0 (v* dt dp)))
		     (v1 (v+ v0 (v* dt dv))))
		 (setf state-lst (append state-lst
					 (list (list p1 v1)))
		       dpos-lst (append dpos-lst
					(list v1))))))
	   (let ((dvel-lst (acceleration everyone state-lst acc-pol-lst (+ t0 dt)))
		 (out-deriv))
	     (dotimes (ndx (length everyone) out-deriv)
	       (setf out-deriv (append out-deriv (list (list (nth ndx dpos-lst)
							     (nth ndx dvel-lst))))))))))
  
  (defun integrate (everyone state0 acc0-pol-lst t0 dt)
    "rk4-integrates the state to t0+dt"
    (let* ((a (evaluate-deriv everyone state0 acc0-pol-lst t0              0))
	   (b (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 dt)       dt       c))
	   (output))
      (dotimes (ndx (length a) output)
	(destructuring-bind (((dpx1 dpy1) (dvx1 dvy1));a
			     ((dpx2 dpy2) (dvx2 dvy2));b
			     ((dpx3 dpy3) (dvx3 dvy3));c
			     ((dpx4 dpy4) (dvx4 dvy4));d
			     ((px py) (vx vy)))
	    (list (pop a) (pop b) (pop c) (pop d)
		  (nth ndx state0))
	  (let ((dpx (* 1/6 (+ dpx1 (* 2 (+ dpx2 dpx3)) dpx4)))
		(dpy (* 1/6 (+ dpy1 (* 2 (+ dpy2 dpy3)) dpy4)))
		(dvx (* 1/6 (+ dvx1 (* 2 (+ dvx2 dvx3)) dvx4)))
		(dvy (* 1/6 (+ dvy1 (* 2 (+ dvy2 dvy3)) dvy4))))
	    (setf output (append output (list (list (list (+ px (* dt dpx)) (+ py (* dt dpy)))
						    (list (+ vx (* dt dvx)) (+ vy (* dt dvy)))
						    (polarize (list dvx dvy))
						    ))))))))))
