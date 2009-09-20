(defvar *options* nil)
(defvar *baddies* nil)
(defvar *guy* nil)
(defvar *class-list* nil)
(defvar *zoom* 10)
(defvar *time* '(0))

(defun init-options ()
  (setf (getf *options* :aa) t))

;(proclaim '(inline pythag distance))

(defun pythag (x y)
  (sqrt (+ (expt x 2)
	   (expt y 2))))

(defun distance (x1 y1 x2 y2)
  (pythag (- x2 x1)
	  (- y2 y1)))

;; This will require some fiddling for joystick input, but not much
(let ((up 0) (down 0) (left 0) (right 0))
  (defun input-key-event (&key key state)
    (case key
      (:sdl-key-q (throw 'game-over 'quit))
      (:sdl-key-e (setf up state))
      (:sdl-key-d (setf down state))
      (:sdl-key-s (setf left state))
      (:sdl-key-f (setf right state))))
  (defun get-input-polar ()
    (let* ((y (- down up))
	   (x (- right left))
	   (r (pythag x y))
	   (r-crop (if (> r 1) 1 r))
	   (theta (atan y x)))
      (list r-crop theta)))))
      

(defun spawn-mortal (&key pos class)
  (let ((mortal (gensym)))
    (setf (get mortal :class) class
	  (get mortal :pos) pos
	  (get mortal :vel) '(0 0)
	  (get mortal :hp) (getf (getf *class-list* class) :health))
    mortal))

(defun attribute (mortal attribute)
  "returns Mortal's Attribute, whether from Mortal's plist or Mortal's class"
  (or (get mortal attribute)
      (getf (getf *class-list* (get mortal :class))
	   attribute)))

(defun define-class (name size speed accel)
  (setf (getf *class-list* name)
	(list :size size :speed speed :accel accel)))

(defun draw-guy (guy)
    (let* ((pos (attribute guy :pos))
	   (xpx (round (* *zoom* (car pos))))
	   (ypx (round (* *zoom* (cadr pos))))
	   (rpx (round (* *zoom* (attribute guy :size)))))
      (sdl-gfx:draw-circle-* xpx ypx rpx :color sdl:*white* :aa (getf *options* :aa))))

(defun update-velocity (guy input delta-t)
  (let* ((ir (car input))
	 (itheta (cadr input))
	 (vel (attribute guy :vel))
	 (vr (car vel))
	 (vtheta (cadr vel))
	 (topspeed (attribute guy :speed))
	 (accel (attribute guy :accel)))
    (if (or (/= (* topspeed ir) vr)
	    (/= ir vr))
	;; (Guy's current | Target) (Velocity | Acceleration) (X component | Y component)
	(let* ((gvx (* vr (cos vtheta)))
	       (gvy (* vr (sin vtheta)))
	       (tvx (* ir (cos itheta) topspeed))
	       (tvy (* ir (sin itheta) topspeed))
	       (tax (- tvx gvx))
	       (tay (- tvy gvy))
	       (ar (pythag tax tay))
	       (atheta (atan tay tax)))
	  (if (> ar (* delta-t accel))
	      (setq ar (* delta-t accel)))
	  (let* ((accel-x (* ar (cos atheta)))
		 (accel-y (* ar (sin atheta)))
		 (new-vx (+ gvx accel-x))
		 (new-vy (+ gvy accel-y))
		 (new-vr (pythag new-vx new-vy))
		 (new-vtheta (atan new-vx new-vy)))
	    (list new-vr new-vtheta)))
	(list vr vtheta))))

(defun move (guy delta-t)
  (destructuring-bind (vr vtheta) (get guy :vel)
    (destructuring-bind (x y) (get guy :pos)
      (let ((vx (* vr (cos vtheta)))
	    (vy (* vr (sin vtheta))))
      (setf (get guy :pos) (list (+ x (* vx delta-t))
				 (+ y (* vy delta-t))))))))


;; The Top Gameloop
(defun play-a-game (&optional (width 800) (height 800))
  (define-class :fighter 1 100 100)
  (setq *guy* (spawn-mortal :pos '(10.1 10) :class :fighter))

  (catch 'game-over
    (sdl:with-init ()
      (sdl:window width height)
      
      (sdl:with-events ()
	(:key-down-event (:key key)
			 (input-key-event :key key :state 1))
	(:key-up-event (:key key)
		       (input-key-event :key key :state 0))
	(:quit-event () t)

	(:idle
	 ()
	 (setq *time* (list (/ (get-internal-real-time) internal-time-units-per-second) (pop *time*)))
	 (let ((delta-t (- (car *time*) (cadr *time*))))
	   (setf (get *guy* :vel) (update-velocity *guy* (get-input-polar) delta-t))
	   (move *guy* delta-t)
	   (print (symbol-plist *guy*))
	   (draw-guy *guy*)
	   (sdl-gfx:draw-circle-* 0 0 10 :color sdl:*red* :aa (getf *options* :aa))
	   (sdl:update-display)
	   (sdl:clear-display sdl:*black*)))))))