(defun spring (k x &optional (v 0) (c 1))
  "returnes force for a damped spring-mass system"
  (- (* (- k) x)
     (* c v)))

;; (defun acceleration-constant (everyone state-lst acc-pol-lst t1)
;;   "returns constant acceleration along x-axis"
;;   (declare (ignore state-lst acc-pol-lst t1))
;;   (let ((accel-lst))
;;     (dotimes (i (length everyone) accel-lst)
;;       (push '(1/2 0) accel-lst))))

;; called by 'evaluate-deriv' four times per physics-loop
(defun acceleration (state0-lst t1)
  "returns everyone's acceleration"
  (declare (ignore t1))	;I might need t1 later, if writing down when things happen
  (let ((force-collision-plst (collision-resolve state0-lst *map*))
	(accel-lst))
    (dolist (state0 state0-lst)
      (with-slots ((guy symbol) pos (vel-current vel) (force-collision force))
	  state0
	;;FIXME: this monstrosity has to be against some sort of rule
	(let* ((force-collision (getf force-collision-plst guy (make-pt)))

	       (mass (attribute guy :mass))
	       (size (attribute guy :size))
	       (leg-str (attribute guy :leg-str))
	       (accelk (attribute guy :accelk))
	       ;; (spd-max (* leg-str (/ mass))))
	       (spd-max (* leg-str size (/ mass)))
	       (run (get-run guy))
	       (run-r (pt-pol-r run))
	       (run-theta (pt-pol-theta run))

	       (target-r (* spd-max run-r))
	       (vel-target (carterize (make-pt-pol target-r run-theta)))
	       (vel-diff (v- vel-current vel-target))
	       (vel-diff-pol (polarize vel-diff))
	       (vel-diff-r (pt-pol-r vel-diff-pol))
	       (vel-diff-theta (pt-pol-theta vel-diff-pol))
	       (force-input-pol (make-pt-pol (spring accelk vel-diff-r)
					     vel-diff-theta))
	       (force-input (carterize force-input-pol))
	       (force-total (v+ force-input force-collision))
	       (acc1 (v* (/ mass) force-total)))
	  (push (cons guy acc1) accel-lst))))
    accel-lst))