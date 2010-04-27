(defun error-report (measured expected &key (places 3) (title "Error report"))
  (let ((error (/ (- measured expected) expected)))
    (fresh-line)
    (format t "~&~%~A~%"
	    title)
    (format t "Expected ~D~&Measured ~D~&Error of ~D~%"
	    (decimalize expected places)
	    (decimalize measured places)
	    (decimalize error places))))

(defun test-accelerate-constant (&key (pos (make-pt-gur)) (vel (make-pt-gur)) (acc (make-pt-gur)))
  (defun game-init ()
    (define-class :mrboring
	:acc-spd 1
	:leg-str 1
	:mass 1
	:accelk 1
	:shape (make-circle :r 1/2)
	)
    (setf *guy* (spawn-mortal :pos pos
			      :vel vel
			      :class :mrboring
			      :control :none
			      )))
  (defun game-timestep ()
    ;; (when (>= (time-now) (+ 10 *time-start*))
    ;;   (throw 'game-over 'timeout))
    )
  (defun game-gameloop ())
  (defun motor (thing vel)
    (declare (ignore thing vel))
    acc))


(defun testcase-elastic-collision ()
  (let ((speed-initial 20)
	(places 3))
    (setf *collision-flavor* '(:guy (:wall (5000 0))))
    (setf *map-load* '(((1.005 2.34)	   ;many complicated bounces
			(48.511 5.97)
			(1.629 47.8)
			(1.005 2.34)
			)))
    ;; (setf *map-load* '(((10 0) (10 100)))) ;one easy bounce
    (test-accelerate-constant :pos (make-pt-gur 20 20) :vel (make-pt-gur (- speed-initial) 0))
    (play-a-game)
    (let* ((speed-final (pythag (attribute *guy* :vel))))
      (error-report speed-final speed-initial :places places)
      )))

(defun testcase-constant-acceleration ()
  (let ((places 3)
	(acc (make-pt-gur 100 0))
	(v0 (make-pt-gur 0 0))
	(p0 (make-pt-gur 10 20)))
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
