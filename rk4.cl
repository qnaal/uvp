;(defun acceleration (state t0)
;  "returns the acceleration at any point"
;  (declare (ignore t0))
;  (destructuring-bind (x v)
;      state
;    10))

;; accel-lst
;;((dvx dvy) (dvx dvy) ...)

;; evaluate-deriv output
;;(((dpx dpy) (dvx dvy)) ((dpx dpy) (dvx dvy)))

;; state-lst
;;(gsym001 ((px py) (vx vy)) gsym002 ((xx xy) (vx vy)))
;;I could pass the gsyms and states as separate lists

;Integrate > Eval > Accel


(defun acceleration (state-lst t1)
  "returns plist of everyone's acceleration"
  (declare (ignore t1))
  (let ((accel-lst))
    (dotimes (i (/ (length state-lst) 2) accel-lst)
      ;;FIXME: converting cart-pol-cart in some cases
      ;;make get-run report cart. coords
      (let ((guy (nth (* 2 i) state-lst)))
	(destructuring-bind (r theta)
	    (get-run guy)
	  (let ((x (* 20 r (cos theta)))
		(y (* 20 r (sin theta))))
	    ;(setf (getf accel-lst guy) (list x y))))))
	    (setf accel-lst (append accel-lst (list (list x y))))))))))


;(flet ((evaluate-deriv (initial-state t0 dt deriv)
;	 "returns (dx dv)"
;	 (destructuring-bind ((x0 v0)
;			      (dx dv))
;	     (list initial-state deriv)
;	   (let ((x1 (+ x0 (* dx dt)))
;		 (v1 (+ v0 (* dv dt))))
;	     (list v1
;		   (acceleration (list x1 v1) (+ t0 dt)))))))

(flet ((evaluate-deriv (initial-state t0 dt &optional (in-deriv () in-deriv-supplied-p))
	 "returns a plist of derivatives in the form
 (guysym ((dpx dpy) (dvx dvy)) ...)"
	 (let ((state-lst)
	       (dpos-lst))
	   (do ((guy-ndx 0 (+ 2 guy-ndx)))
	       ((= guy-ndx (length initial-state)))
	     (destructuring-bind (guy 
				  ((px0 py0) (vx0 vy0))
				  ((dpx dpy) (dvx dvy)))
		 (list (nth guy-ndx initial-state)
		       (nth (1+ guy-ndx) initial-state)
		       (if in-deriv-supplied-p
			   (nth (/ guy-ndx 2) in-deriv)
			   '((0 0) (0 0))))
	       (declare (ignore px0 py0 dpx dpy))
	       (let (;;(px1 (+ px0 (* dpx dt)))
		     ;;(py1 (+ py0 (* dpy dt)))
		     (vx1 (+ vx0 (* dvx dt)))
		     (vy1 (+ vy0 (* dvy dt))))
		 (setf state-lst (append state-lst
					 (list guy)
					 (list (list vx1 vy1)))
		       dpos-lst (append dpos-lst
					(list (list vx1 vy1)))))))
	   (let ((dvel-lst (acceleration state-lst (+ t0 dt)))
		 (out-deriv))
	     (dotimes (ndx (length dpos-lst) out-deriv)
	       (setf out-deriv (append out-deriv (list (list (nth ndx dpos-lst)
							     (nth ndx dvel-lst))))))))))
  
;  (defun integrate (state t0 dt)
;    "rk4-integrates the state t0+dt"
;    (let* ((a (evaluate-deriv state t0 0 ;;state)) ;;this should not have worked i think
;	   (b (evaluate-deriv state (+ t0 (/ dt 2)) (/ dt 2) a))
;	   (c (evaluate-deriv state (+ t0 (/ dt 2)) (/ dt 2) b))
;	   (d (evaluate-deriv state (+ t0 dt) dt c)))
;      (destructuring-bind ((xa va)
;			   (xb vb)
;			   (xc vc)
;			   (xd vd))
;	  (list a b c d)
;	(let ((dx (* dt 1/6 (+ xa (* 2 (+ xb xc)) xd)))
;	      (dv (* dt 1/6 (+ va (* 2 (+ vb vc)) vd))))
;	  (mapcar #'+ state (list dx dv)))))))

  (defun integrate (state0-lst t0 dt)
    "rk4-integrates the state to t0+dt"
    (let* ((a (evaluate-deriv state0-lst t0 0))
	   (b (evaluate-deriv state0-lst (+ t0 (/ dt 2)) (/ dt 2) a))
	   (c (evaluate-deriv state0-lst (+ t0 (/ dt 2)) (/ dt 2) b))
	   (d (evaluate-deriv state0-lst (+ t0 dt) dt c))
	   (output))
      (dotimes (ndx (length a) output)
	(destructuring-bind (((dpx1 dpy1) (dvx1 dvy1));a
			     ((dpx2 dpy2) (dvx2 dvy2));b
			     ((dpx3 dpy3) (dvx3 dvy3));c
			     ((dpx4 dpy4) (dvx4 dvy4));d
			     ((px py) (vx vy)))
	    (list (pop a) (pop b) (pop c) (pop d)
		  (nth (1+ (* 2 ndx)) state0-lst))
	  (let ((dpx (* dt 1/6 (+ dpx1 (* 2 (+ dpx2 dpx3)) dpx4)))
		(dpy (* dt 1/6 (+ dpy1 (* 2 (+ dpy2 dpy3)) dpy4)))
		(dvx (* dt 1/6 (+ dvx1 (* 2 (+ dvx2 dvx3)) dvx4)))
		(dvy (* dt 1/6 (+ dvy1 (* 2 (+ dvy2 dvy3)) dvy4))))
	    (setf output (append output (list (list (list (+ px dpx) (+ py dpy))
						    (list (+ vx dvx) (+ vy dvy))))))))))))
