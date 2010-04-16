(load "test.cl")

(defun game-baddies ()
  (defun game-init ()
    ;;(setf *map-load* '(((15 15) (20 20) (10 55) (30 20))))
    (setf *map-load* '(((15 15) (20 20) (10 55) (30 20)) ((50 50) (50 60) (60 60) (60 50)) ((100 100) (-100 100) (-100 -100) (100 -100) (100 100))))
    (setf *collision-flavor* '(:guy (:guy (1000 20) :wall (4000 40))))
    ;; (setf *collision-flavor* '(:guy (:guy (2000 30) :wall (10000 50))))
    ;; (setf *collision-flavor* '(:guy (:guy (2000 30) :wall (20000 100))))
    ;; (define-class :fighter 1 20 80 2 4)
    ;; (define-class :baddie-swarmer 1/2 1 100 1/2 1/2)
    (define-class :fighter
	:acc-spd 40
	:leg-str 80
	:mass 2
	:accelk 4
	:shape (make-circle :r 1))
    (define-class :baddie-swarmer
	:acc-spd 10
	:leg-str 50
	:mass 1
	:accelk 1
	:shape (make-circle :r 1/2))
    (define-projectile :arrow
	:mass 1/2
	:speed 500
	:shape (make-circle :r 1/10))
    (setq *guy* (spawn-mortal :pos (make-pt 10 10)
			      :class :fighter
			      :control :input)))

  (defun game-timestep ())

  (defun game-gameloop ()
    (if (< (length *baddies*)
	   10)
	(push (spawn-mortal :pos (v+ (make-pt 51 51) (make-pt (random 8.0) (random 8.0)))
			    :class :baddie-swarmer
			    :control :ai)
	      *baddies*))))
