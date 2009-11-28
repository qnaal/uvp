;; accel-lst
;;((dvx dvy) (dvx dvy) ...)

;; evaluate-deriv output
;;(((dpx dpy) (dvx dvy)) ((dpx dpy) (dvx dvy)))

;; state-lst
;;(gsym001 ((px py) (vx vy)) gsym002 ((xx xy) (vx vy)))
;;I could pass the gsyms and states as separate lists

;Integrate > Eval > Accel



(flet ((evaluate-deriv (everyone initial-state acc-pol-lst t0 dt &optional (in-deriv () in-deriv-supplied-p))
	 "returns a plist of derivatives in the form
 (guysym ((dpx dpy) (dvx dvy)) ...)"
	 (let ((state-lst)
	       (dpos-lst))
;	   (do ((guy-ndx 0 (+ 2 guy-ndx)))
;	       ((= guy-ndx (length initial-state)))
	   (dotimes (guy-ndx (length everyone))
	     (destructuring-bind (((px0 py0) (vx0 vy0))
				  ((dpx dpy) (dvx dvy)))
		 (list (nth guy-ndx initial-state)
		       (if in-deriv-supplied-p
			   (nth guy-ndx in-deriv)
			   '((0 0) (0 0))))
	       (declare (ignore px0 py0 dpx dpy))
	       (let (;;(px1 (+ px0 (* dpx dt)))
		     ;;(py1 (+ py0 (* dpy dt)))
		     (vx1 (+ vx0 (* dvx dt)))
		     (vy1 (+ vy0 (* dvy dt))))
		 (setf state-lst (append state-lst
					 ;(list guy)
					 (list (list vx1 vy1)))
		       dpos-lst (append dpos-lst
					(list (list vx1 vy1)))))))
	   (let ((dvel-lst (acceleration everyone state-lst acc-pol-lst (+ t0 dt)))
		 (out-deriv))
	     (dotimes (ndx (length dpos-lst) out-deriv)
	       (setf out-deriv (append out-deriv (list (list (nth ndx dpos-lst)
							     (nth ndx dvel-lst))))))))))
  
  (defun integrate (everyone state0 acc0-pol-lst t0 dt)
    "rk4-integrates the state to t0+dt"
    (let* ((a (evaluate-deriv everyone state0 acc0-pol-lst t0 0))
	   (b (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv everyone state0 acc0-pol-lst (+ t0 dt) dt c))
	   (output))
      (dotimes (ndx (length a) output)
	(destructuring-bind (((dpx1 dpy1) (dvx1 dvy1));a
			     ((dpx2 dpy2) (dvx2 dvy2));b
			     ((dpx3 dpy3) (dvx3 dvy3));c
			     ((dpx4 dpy4) (dvx4 dvy4));d
			     ((px py) (vx vy)))
	    (list (pop a) (pop b) (pop c) (pop d)
		  ;;(nth (1+ (* 2 ndx)) state0-lst))
		  (nth ndx state0))
	  (let ((dpx (* 1/6 (+ dpx1 (* 2 (+ dpx2 dpx3)) dpx4)))
		(dpy (* 1/6 (+ dpy1 (* 2 (+ dpy2 dpy3)) dpy4)))
		(dvx (* 1/6 (+ dvx1 (* 2 (+ dvx2 dvx3)) dvx4)))
		(dvy (* 1/6 (+ dvy1 (* 2 (+ dvy2 dvy3)) dvy4)))
)
	    (setf output (append output (list (list (list (+ px (* dt dpx)) (+ py (* dt dpy)))
						    (list (+ vx (* dt dvx)) (+ vy (* dt dvy)))
						    (polarize (list dvx dvy))
						    ))))))))))
