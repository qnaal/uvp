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

(defun test-elastic-collision ()
  (let ((speed-initial 20)
	(places 3))
    (defparameter *collision-flavor* '(:guy (:wall (1000 0))))
    (setf *map-load* '(((10 0) (10 100))))
    (test-accelerate-constant :pos (make-pt 20 20) :vel (make-pt (- speed-initial) 0))
    (play-a-game)
    (let* ((speed-final (pythag (attribute *guy* :vel)))
	   (speed-error (/ (- speed-final speed-initial) speed-initial)))
      (fresh-line)
      (format t "Started with speed ~D~&Ended with speed ~D~&Relative error is ~D"
	      speed-initial
	      (decimalize speed-final places)
	      (decimalize speed-error places))
      )))
