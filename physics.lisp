;; (defun acceleration-constant (state0-lst t1)
;;   "returns constant acceleration along x-axis"
;;   (declare (ignore t1))
;;   (let ((accel-lst))
;;     (dotimes (i (length state0-lst) accel-lst)
;;       (let ((thing (state-symbol (elt state0-lst i))))
;; 	(push (cons thing (make-pt 1/2 0)) accel-lst)))))

(defun motor (thing vel)
  "calculates the force thing applies to itself"
  (case (attribute thing :motor)
    (:motor
     (let ((run (get-run thing))
	   (mass (attribute thing :mass))
	   (leg-str (attribute thing :leg-str))
	   (accelk (attribute thing :accelk)))
       (with-slots ((run-r r) (run-theta theta)) run
	 (let* ((spd-max (* leg-str (/ mass))) ;this needs thought about more- should it take size into account etc
		(target-r (* spd-max run-r))
		(vel-target (carterize (make-pt-pol-gur target-r run-theta)))
		(vel-diff-pol (polarize (v- vel vel-target))))
	   ;; (with-slots ((vel-diff-r r) (vel-diff-theta theta)) (polarize vel-diff)
	   (let ((vel-diff-r (pt-pol-gur-r vel-diff-pol))
		 (vel-diff-theta (pt-pol-gur-theta vel-diff-pol)))
	     (let* ((force-pol (make-pt-pol-gur (spring accelk vel-diff-r) ;this has a strange mix of board and gur units
						 vel-diff-theta))
		    (force (carterize force-pol)))
	       force))))))
    (otherwise (make-pt))))

;; called by 'evaluate-deriv' four times per physics-loop
(defun acceleration (state0-lst t1)
  "returns everyone's acceleration"
  (declare (ignore t1))	;I might need t1 later, if writing down when things happen
  (let ((force-collision-plst (collision-resolve state0-lst *map*))
	(accel-lst))
    (dolist (state0 state0-lst)
      (with-slots ((guy symbol) pos (vel-current vel))
	  state0
	(let* ((force-collision (getf force-collision-plst guy (make-pt)))
	       (force-motor (motor guy vel-current))
	       (mass (attribute guy :mass))
	       (force-total (v+ force-motor force-collision))
	       (acc1 (v* (/ mass) force-total)))
	  (push (cons guy acc1) accel-lst))))
    accel-lst))

(defun acceleration-constant (state0-lst t1)
  "return constant positive acceleration along x - for testing purposes"
  (declare (ignore t1))
  (let ((accel-lst))
    (dolist (state0 state0-lst accel-lst)
      (with-slots (symbol) state0
	(push (cons symbol (make-pt-gur 1 0)) accel-lst)))))
