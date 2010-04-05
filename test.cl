(defun error-report (measured expected &key (places 3) (title "Error report"))
  (let ((error (/ (- measured expected) expected)))
    (fresh-line)
    (format t "~&~%~A~%"
	    title)
    (format t "Expected ~D~&Measured ~D~&Error of ~D~%"
	    (decimalize expected places)
	    (decimalize measured places)
	    (decimalize error places))))

(defun test-accelerate-constant (&key (pos (make-pt)) (vel (make-pt)) (acc (make-pt)))
  (defun game-init ()
    (define-class :mrboring
	:size 1
	:acc-spd 1
	:leg-str 1
	:mass 1
	:accelk 1)
    (setf *guy* (spawn-mortal :pos pos
			      :vel vel
			      :class :mrboring
			      :control :none
			      )))
  (defun game-timestep ())
  (defun game-gameloop ())
  (defun motor (thing vel)
    (declare (ignore thing vel))
    acc))


(defun testcase-elastic-collision ()
  (let ((speed-initial 20)
	(places 3))
    (defparameter *collision-flavor* '(:guy (:wall (1000 0))))
    (setf *map-load* '(((10 0) (10 100))))
    (test-accelerate-constant :pos (make-pt 20 20) :vel (make-pt (- speed-initial) 0))
    (play-a-game)
    (let* ((speed-final (pythag (attribute *guy* :vel))))
      (error-report speed-final speed-initial :places places)
      )))

(defun testcase-constant-acceleration ()
  (let ((places 3)
	(acc (make-pt 100 0))
	(v0 (make-pt 0 0))
	(p0 (make-pt 10 20)))
    (setf *map-load* nil)
    (test-accelerate-constant :pos p0 :vel v0 :acc acc)
    (play-a-game)
    (let* ((dt (- (time-now) *time-start*))
	   (p1 (attribute *guy* :pos))
	   (dp (v- p1 p0))
	   (dp-accepted (v+ (v* dt v0)
			    (v* (* 1/2 (expt dt 2)) acc)))
	   (v1 (attribute *guy* :vel))
	   (dv (v- v1 v0))
	   (dv-accepted (v* dt acc)))
      (error-report (pythag dp) (pythag dp-accepted) :places places :title "Change in Position")
      (error-report (pythag dv) (pythag dv-accepted) :places places :title "Chance in Velocity")
      )))
