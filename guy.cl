(asdf:oos 'asdf:load-op 'lispbuilder-sdl)
(asdf:oos 'asdf:load-op 'lispbuilder-sdl-gfx)

(defvar *options* nil)
(defvar *baddies* nil)
(defvar *guy* nil)
(defvar *class-list* nil)
(defvar *zoom* 10)
;; (defvar *time* '(0))
;(defvar *map* '(((15 15) (20 20) (10 55) (30 20))))
(defvar *map* '(((15 15) (20 20) (10 55) (30 20)) ((50 50) (50 60) (60 60) (60 50))))
(defparameter *collision-flavor* '(:guy (:guy (2000 30) :wall (10000 50))))
(defvar *mapbounds*)
(defvar *particles* nil)
(defvar *buffer* (/ 1000))

(defun init-options ()
  (setf (getf *options* :aa) t))

;;(proclaim '(inline pythag distance collision-circle-circle))
(load "src/uvp/vector-math.cl")

;; (defun spawn-particle (&key pos theta birth)
;;   (push (list pos theta birth) *particles*))

;; This will require some fiddling for joystick input, but not much
(let ((up 0) (down 0) (left 0) (right 0))
  (defun input-key-event (&key key state)
    (print (list key state))
    (case key
      (1 (print 'FIRE))
      (:sdl-key-q (throw 'game-over 'quit))
      (:sdl-key-e (setf up state))
      (:sdl-key-d (setf down state))
      (:sdl-key-s (setf left state))
      (:sdl-key-f (setf right state))))
  (defun get-input-polar ()
    (let* ((y (- down up))
	   (x (- right left))
	   (r (pythag (list x y)))
	   (r-crop (if (> r 1) 1 r))
	   (theta (atan y x)))
      (list r-crop theta))))
      
;should include a key for any mods (from magic, leveling, etc)
(defun spawn-mortal (&key pos class control)
  (let ((mortal (gensym)))
    (setf (get mortal :class) class
	  (get mortal :pos) pos
	  (get mortal :vel-pol) '(0 0)
	  (get mortal :acc-pol) '(0 0)
	  (get mortal :control) control
	  (get mortal :hp) (getf (getf *class-list* class) :health))
    mortal))

(defun attribute (mortal attribute)
  "returns Mortal's Attribute, whether from Mortal's plist or Mortal's class"
  (or (get mortal attribute)
      (getf (getf *class-list* (get mortal :class))
	   attribute)))

;should macro this from a list of attributes
(defun define-class (name size acc-spd leg-str mass accelk)
  (setf (getf *class-list* name)
	(list :size size :acc-spd acc-spd :leg-str leg-str :mass mass :accelk accelk)))

(defun project (x)
  (round (* x *zoom*)))

(defun project-pt (x-y-lst)
  (let ((x (car x-y-lst))
	(y (cadr x-y-lst)))
    (sdl:point :x (* x *zoom*)
	       :y (* y *zoom*))))

(defun draw-guy (guy)
    (let* ((pos (attribute guy :pos))
	   (pt (project-pt pos))
	   (r (project (attribute guy :size))))
      (sdl-gfx:draw-circle pt r :color sdl:*white* :aa (getf *options* :aa))))
(defun draw-poly (poly color)
					;(sdl-gfx:draw-shape poly :color color :aa (getf *options* :aa))
  ;;FIXME: temporary workaround because draw-shape inexplicably doesn't do antialiasing
  (do ((line-index 0 (+ 1 line-index))) ((= line-index (1- (length poly))))
    (let ((p1 (nth     line-index  poly))
	  (p2 (nth (1+ line-index) poly)))
      (sdl-gfx:draw-line p1 p2 :color color :aa (getf *options* :aa)))))
(defun draw-map (map color)
  (dotimes (poly-index (length map))
    (let ((poly (nth poly-index map))
	  (sdl-poly))
      (dotimes (pt-index (length poly))
	(push (project-pt (nth pt-index poly)) sdl-poly))
      (draw-poly sdl-poly color))))

(defun get-run (mortal)
  (case (attribute mortal :control)
    (:ai (destructuring-bind ((x y) (target-x target-y))
	     (list (attribute mortal :pos) (attribute *guy* :pos))
	   (list 1 (atan (- target-y y)
			 (- target-x x)))))
    (:input (get-input-polar))))

(load "src/uvp/physics.cl")
(load "src/uvp/collision-spring.cl")
(load "src/uvp/rk4.cl")

(defun movement-debug (mortal)
  "radar thing"
  (let ((vel (carterize (attribute mortal :vel-pol)))
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
    (sdl:draw-pixel-* (+ 50 (round (* 30 (car  target))))
		      (+ 50 (round (* 30 (cadr target)))) :color sdl:*blue*)
    (sdl:draw-pixel-* (+ 50 (round (* 02 (car  vel))))
		      (+ 50 (round (* 02 (cadr vel)))) :color sdl:*green*)
    (sdl:draw-pixel-* (+ 50 (round (* 1/2 (car  acc))))
		      (+ 50 (round (* 1/2 (cadr acc)))) :color sdl:*red*)))
  

;; The Top Gameloop
(defun play-a-game (&optional (width 800) (height 800))
  (print (/ (get-internal-real-time)
	    internal-time-units-per-second))
  (define-class :fighter 1 20 80 2 4)
  (define-class :baddie-swarmer 1/2 1 100 1/2 1/2)
  (setq *guy* (spawn-mortal :pos '(10 10)
			    :class :fighter
			    :control :input)
	*baddies* ()
	;*mapbounds* (generate-bounding-circles *map*)
	)
  
  (catch 'game-over
    (sdl:with-init ()
      ;; (sdl:window width height)
      (sdl:window width height :fps (make-instance 'sdl:fps-timestep :dt 10 :max-dt 100))
      (setf (sdl:frame-rate) 0)
      (sdl:with-events ()
	(:key-down-event (:key key)
			 (input-key-event :key key :state 1))
	(:key-up-event (:key key)
		       (input-key-event :key key :state 0))
	(:mouse-button-down-event (:button button)
			 (input-key-event :key button :state 1))
	(:quit-event () t)
	(:idle
	 (sdl:with-timestep 
	   (let ((state-lst)
		 (acc-pol-lst)
		 (everyone (append *baddies* (list *guy*)))
		 (intgr-out))
	     (dolist (guy everyone)
	       (let* ((pos (attribute guy :pos))
		      (vel-pol (attribute guy :vel-pol))
		      (vel (carterize vel-pol))
		      (state (list pos vel))
		      (acc-pol (attribute guy :acc-pol)))
		 (setf state-lst (append state-lst (list state))
		     acc-pol-lst (append acc-pol-lst (list acc-pol)))))
	     (let* ((dt (/ (sdl:dt) 1000))
		    (t1 (/ (sdl:system-ticks) 1000))
		    (t0 (- t1 dt)))
	       (setf intgr-out (integrate everyone state-lst acc-pol-lst t0 dt)))
	     (dolist (guy everyone)
	       (destructuring-bind (pos vel acc-pol)
		   (pop intgr-out)
		 (let ((vel-pol (polarize vel)))
		   (setf (get guy :pos) pos
			 (get guy :vel-pol) vel-pol
			 (get guy :acc-pol) acc-pol))))))

	 (print (list 'fps (round (sdl:average-fps))))
	 (if (< (length *baddies*)
		20)
	     (push (spawn-mortal :pos (v+ '(51 51) (list (random 8.0) (random 8.0)))
				 :class :baddie-swarmer
				 :control :ai)
		   *baddies*))
	 (movement-debug *guy*)

	 (draw-guy *guy*)
	 (dotimes (baddie-index (length *baddies*))
	   (draw-guy (nth baddie-index *baddies*)))
	 (draw-map *map* sdl:*magenta*)
	 (sdl:update-display)
	 (sdl:clear-display sdl:*black*)
	 )))))



(defun test-loop (&optional (width 800) (height 800))
  (print (/ (get-internal-real-time)
	    internal-time-units-per-second))
  (define-class :fighter 1 20 40 2 4)
  (define-class :baddie-swarmer 1/2 1 50 1/2 1/2)
  (setq *guy* (spawn-mortal :pos '(10 10)
			    :class :fighter
			    :control :input)
	*baddies* ()
	;*mapbounds* (generate-bounding-circles *map*)
	)
  
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
