(asdf:oos 'asdf:load-op 'lispbuilder-sdl)
(asdf:oos 'asdf:load-op 'lispbuilder-sdl-gfx)

(defvar *options* nil)
(defvar *baddies* nil)
(defvar *guy* nil)
(defvar *class-list* nil)
(defvar *zoom* 10)
(defvar *time* '(0))
(defvar *mapbounds*)
(defvar *particles* nil)
(defvar *buffer* (/ 1000))
(defvar *time-start*)
(defvar *map-load* nil)
(defvar *collision-flavor*)

(load "vector-math.cl")

(defun time-now ()
  (car *time*))
(defun time-prv ()
  (cdr *time*))
(defun time-adv ()
  (setf *time* (push (/ (get-internal-real-time)
			internal-time-units-per-second)
		     (car *time*))))

;; this would look good in a hashtable
(defun attribute (mortal attribute)
  "returns Mortal's Attribute, whether from Mortal's plist or Mortal's class"
  (or (get mortal attribute)
      (getf (getf *class-list* (get mortal :class))
	   attribute)))

(defun decimalize (number places)
  (let ((precision (expt 10 places)))
    (float (/ (round number (/ precision))
	      precision))))

(defstruct particle
  type pos theta birth)

(defun spawn-particle (guy type theta)
  (let ((pos (attribute guy :pos))
	(now (time-now)))
    (make-particle :type type :pos pos :theta theta :birth now)))

(defun advance-particle (particle dt)
  (let* ((pos (particle-pos particle))
	 (theta (particle-theta particle))
	 (speed 100)
	 (traveled (carterize (make-pt-pol (* speed dt) theta)))
	 )
    (setf (particle-pos particle) (v+ pos traveled))))

(load "graphics.cl")

(defun generate-map (map)
  (let ((map-gen))
    (dolist (poly map)
      (let ((poly-gen))
	(dolist (lst-pt poly)
	  (destructuring-bind (x y) lst-pt
	    (push (make-pt x y) poly-gen)))
	(push poly-gen map-gen)))
    map-gen))
(defvar *map* (generate-map *map-load*))

(defun init-options ()
  (setf (getf *options* :aa) t))

;; This will require some fiddling for joystick input, but not much
(let ((up 0) (down 0) (left 0) (right 0))
  (defun input-key-event (&key key state x y)
    (print (list key state))
    (case key
      (1 (print 'FIRE)
	 (let ((aim-rel (v- (make-pt (unproject x) (unproject y))
			    (attribute *guy* :pos))))
	   (push (spawn-particle *guy*
				  :arrow
				  (azimuth aim-rel))
		 *particles*))
	 )
      (:sdl-key-q (throw 'game-over 'quit))
      (:sdl-key-e (setf up state))
      (:sdl-key-d (setf down state))
      (:sdl-key-s (setf left state))
      (:sdl-key-f (setf right state))))
  (defun get-input-polar ()
    (let* ((y (- down up))
	   (x (- right left))
	   (r (pythag (make-pt x y)))
	   (r-crop (if (> r 1) 1 r))
	   (theta (atan y x)))
      (make-pt-pol r-crop theta))))

;; should include a key for any mods (from magic, leveling, etc)
(defun spawn-mortal (&key pos class control (vel (make-pt)))
  (let ((mortal (gensym)))
    (setf (get mortal :class) class
	  (get mortal :pos) pos
	  (get mortal :safe) pos
	  (get mortal :vel) vel
	  (get mortal :acc-pol) (make-pt-pol)
	  (get mortal :control) control
	  (get mortal :hp) (getf (getf *class-list* class) :health))
    mortal))

;; should macro this from a list of attributes
(defun define-class (name &key size acc-spd leg-str mass accelk)
  (setf (getf *class-list* name)
	(list :size size :acc-spd acc-spd :leg-str leg-str :mass mass :accelk accelk)))

(defun get-run (mortal)
  "returns a polar vector of the direction mortal wants to go, scaled from 0 to 1 based on how much it wants to go there"
  (case (attribute mortal :control)
    (:ai (let* ((pos (attribute mortal :pos))
		(x (pt-x pos))
		(y (pt-y pos))
		(target-pt (attribute *guy* :pos))
		(target-x (pt-x target-pt))
		(target-y (pt-y target-pt)))
	   (make-pt-pol 1 (atan (- target-y y)
				(- target-x x)))))
    (:input (get-input-polar))
    (:none (make-pt-pol))))

(defstruct state
  (symbol 0 :type symbol)
  (pos () :type pt)
  (safe ())
  (vel () :type pt))

(load "collision-spring.cl")
(load "physics.cl")
(load "rk4.cl")
(load "mods.cl")
(game-baddies)

(defun movement-debug (mortal)
  "radar thing"
  (let ((vel (attribute mortal :vel))
	(acc (carterize (attribute mortal :acc-pol)))
					;(contact (carterize *debug-contact*))
	;; (contact *debug-contact*)
	;; (contact-min (carterize (list 20 (cadr (polarize *debug-contact*)))))
	(target (carterize (get-run mortal))))
    (sdl:draw-circle-* 50 50 50 :color sdl:*blue*)
    (sdl:draw-pixel-* 50 50 :color sdl:*white*)
    ;; (sdl:draw-line-* 50 50
    ;; 		     (+ 50 (round (* 1 (car  contact-min))))
    ;; 		     (+ 50 (round (* 1 (cadr contact-min)))) :color sdl:*yellow*)
    ;; (sdl:draw-line-* 50 50
    ;; 		     (+ 50 (round (* 1 (car  contact))))
    ;; 		     (+ 50 (round (* 1 (cadr contact)))) :color sdl:*magenta*)
    (sdl:draw-pixel-* (+ 50 (round (* 30 (pt-x target))))
		      (+ 50 (round (* 30 (pt-y target)))) :color sdl:*blue*)
    (sdl:draw-pixel-* (+ 50 (round (* 02 (pt-x vel))))
		      (+ 50 (round (* 02 (pt-y vel)))) :color sdl:*green*)
    (sdl:draw-pixel-* (+ 50 (round (* 1/2 (pt-x acc))))
		      (+ 50 (round (* 1/2 (pt-y acc)))) :color sdl:*red*)))

;; The Top Gameloop
(defun play-a-game (&optional (width 800) (height 800))
  (print (/ (get-internal-real-time)
	    internal-time-units-per-second))
  (game-init)
  (setf *map* (generate-map *map-load*))
  (setq *particles* nil)

  (time-adv)
  (setf *time-start* (time-now))

  (catch 'game-over
    (sdl:with-init ()
      (sdl:window width height :fps (make-instance 'sdl:fps-timestep :dt 20 :max-dt 100))
      (setf (sdl:frame-rate) 0)
      (sdl:with-events ()
	(:key-down-event (:key key)
			 (input-key-event :key key :state 1))
	(:key-up-event (:key key)
		       (input-key-event :key key :state 0))
	(:mouse-button-down-event (:button button :x x :y y)
				  (input-key-event :key button :state 1 :x x :y y))
	(:quit-event () t)
	(:idle

	 (sdl:with-timestep 
	   (time-adv)
	   ;; physics and such important stuff go here
	   ;; perhaps put all this in some sort of Grand Unified Timestep Function perhaps?
	   (let* ((everyone (append *baddies* (list *guy*))) ;TODO: make a global vector for 'everyone's symbols
		  (state0-lst))
	     ;; generate states
	     (dolist (thing everyone)
	       (let ((pos (attribute thing :pos))
		     (safe (attribute thing :safe))
		     (vel (attribute thing :vel)))
		 (push (make-state :symbol thing
				   :pos pos
				   :safe safe
				   :vel vel)
		       state0-lst)))
	     ;; integrate to state1, then move things
	     (let* ((dt (/ (sdl:dt) 1000))
		    (t1 (/ (sdl:system-ticks) 1000))
		    (t0 (- t1 dt))
		    (state1-lst (integrate-rk4 state0-lst t0 dt)))
	       (dolist (state1 state1-lst)
		 (with-slots ((thing symbol) pos vel) state1
		   (let ((pos0 (get thing :pos))
			 (safe (get thing :safe)))
		     (when (safe-check pos0 safe *map*)
		       (setf (get thing :safe) pos0)))
		   (setf (get thing :pos) pos
			 (get thing :vel) vel))))
	     (game-timestep)
	     ;; (print (list 'fps (round (sdl:average-fps))))
	     )
	   )
	 ;; everything but the physics
	 (movement-debug *guy*)
	 (game-gameloop)

	 (draw-guy *guy*)
	 (dolist (baddie *baddies*)
	   (draw-guy baddie))
	 (dolist (part *particles*)
	   (draw-particle part))
	 (draw-map *map* sdl:*magenta*)
	 (sdl:update-display)
	 (sdl:clear-display sdl:*black*)
	 ))))
  t)



(defun test-loop (&optional (width 800) (height 800))
  (print (/ (get-internal-real-time)
	    internal-time-units-per-second))
  ;; (define-class :fighter 1 20 40 2 4)
  ;; (define-class :baddie-swarmer 1/2 1 50 1/2 1/2)
  (setq *guy* (spawn-mortal :pos (make-pt 10 10)
			    :class :fighter
			    :control :input)
	*baddies* ())

  (let ((timestep 0)
	(main 0))
    (catch 'game-over
      (sdl:with-init ()
	;; (sdl:window width height)
	(sdl:window width height :fps (make-instance 'sdl:fps-timestep :dt 10 :max-dt 100))
	(setf (sdl:frame-rate) 0)
	(sdl:with-events ()
	  (:key-down-event ();:button button)
			   (throw 'game-over 'quit))
	  (:mouse-button-down-event ();:button button)
				    (throw 'game-over 'quit))
	  (:quit-event () t)
	  (:idle
	   (sdl:with-timestep 

	     (incf timestep)
	     (print (list 'ts timestep)))

	   (sleep .1)
	   (incf main)
	   (print (list 'main main))
	   (print (list 'fps (round (sdl:average-fps))))
	   (sdl:update-display)
	   (sdl:clear-display sdl:*black*)

	   ))))))
