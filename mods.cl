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
    (define-type :guy :shape :circle)
    (define-class :fighter :size 1 :acc-spd 40 :leg-str 80 :mass 2 :accelk 4)
    (define-class :baddie-swarmer :size 1/2 :acc-spd 10 :leg-str 100 :mass 1 :accelk 1)
    (define-type :arrow :shape :circle :size 1/10 :mass 1/2)
    (setq *guy* (spawn-mortal :pos (make-pt 10 10)
			      :class :fighter
			      :control :input)))

  (defun game-timestep ())

  (defun game-gameloop ()
    (if (< (length *baddies*)
	   0)
	(push (spawn-mortal :pos (v+ (make-pt 51 51) (make-pt (random 8.0) (random 8.0)))
			    :class :baddie-swarmer
			    :control :ai)
	      *baddies*))))