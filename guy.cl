(defvar *options* nil)
(defvar *baddies* nil)
(defvar *guy* nil)
(defvar *class-list* nil)
(defvar *zoom* 10)
(defvar *time* '(0))
(defvar *map* '(((15 15) (20 20) (10 55) (30 20))))
(defvar *mapbounds*)

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
      (list r-crop theta))))
      

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
	(let* ((gvx (* vr (cos vtheta)));-
	       (gvy (* vr (sin vtheta)));-
	       (tvx (* ir (cos itheta) topspeed));g
	       (tvy (* ir (sin itheta) topspeed));g
	       (tax (- tvx gvx));g
	       (tay (- tvy gvy));g
	       (ar (pythag tax tay));g
	       (atheta (atan tay tax)));g
	  ;(print (list tax tay))
	  (if (> ar (* delta-t accel))
	      (setq ar (* delta-t accel)))
	  (let* ((accel-x (* ar (cos atheta)))
		 (accel-y (* ar (sin atheta)))
		 (new-vx (+ gvx accel-x))
		 (new-vy (+ gvy accel-y))
		 (new-vr (pythag new-vx new-vy))
		 (new-vtheta (atan new-vy new-vx)))
	    (list new-vr new-vtheta)))
	(list vr vtheta))))

(defun move (guy delta-t)
  (destructuring-bind (vr vtheta) (get guy :vel)
    (destructuring-bind (x y) (get guy :pos)
      (let ((vx (* vr (cos vtheta)))
	    (vy (* vr (sin vtheta))))
      (setf (get guy :pos) (list (+ x (* vx delta-t))
				 (+ y (* vy delta-t))))))))

(defun project (x)
  (round (* x *zoom*)))

(defun project-pt (x-y-lst)
  (let ((x (car x-y-lst))
	(y (cadr x-y-lst)))
    (sdl:point :x (* x *zoom*)
	       :y (* y *zoom*))))

(defun polarize-points (points-list &optional (rotate 0) &aux polar-list)
  (dotimes (point-index (/ (length points-list) 2))
    (let ((x (nth     (* 2 point-index)  points-list))
          (y (nth (1+ (* 2 point-index)) points-list)))
      (push (pythag x y) polar-list)
      (push (+ (atan y x) rotate) polar-list)))
  (nreverse polar-list))

(defun rotate-points (points-list rotate &aux polar-list cart-list)
  (setf polar-list (polarize-points points-list rotate))
  (dotimes (point-index (/ (length points-list) 2))
    (let ((r     (nth     (* 2 point-index)  polar-list))
          (theta (nth (1+ (* 2 point-index)) polar-list)))
      (push (* r (cos theta)) cart-list)
      (push (* r (sin theta)) cart-list)))
  (nreverse cart-list))

(flet ((cross (x1 y1 x2 y2) (- (* x1 y2)
			       (* x2 y1))))
  (defun collision-line-line (ax1 ay1 ax2 ay2 bx1 by1 bx2 by2)
    "returns where alone line A they intersect, (< 0 x 1)"
    (if (not (= 0 (cross (- ax2 ax1) (- ay2 ay1)
			 (- bx2 bx1) (- by2 by1))))
	;;Xhit is where along line X the intersection is
	(let ((bhit (/ (cross (- bx1 ax1) (- by1 ay1)
			      (- ax2 ax1) (- ay2 ay1))
		       (cross (- ax2 ax1) (- ay2 ay1)
			      (- bx1 bx1) (- by2 by1))))
	      (ahit (/ (cross (- bx1 ax1) (- by1 ay1)
			      (- bx2 bx1) (- by2 by1))
		       (cross (- ax2 ax1) (- ay2 ay1)
			      (- bx2 bx1) (- by2 by1)))))
	  (if (and (< 0 bhit 1)    
		   (< 0 ahit 1))
	      ahit)))))

(defun collision-line-circle (x1 y1 x2 y2 xc yc rc &key resolve)
  "detects whether line (x1,y1,x2,y2) intersects with circle (xc,yx,rc).
If resolve, gives the corrected xc,yc as well as the angle of the line, by buffer resolve."
  (let* ((xd (- x2 x1))
         (yd (- y2 y1))
         (rotate (atan yd xd)))
    (destructuring-bind (rx1 ry1 rx2 ry2 rxc ryc)
        (rotate-points (list x1 y1 x2 y2 xc yc) (- rotate))
      (if (and (< (- ry1 rc) ryc (+ ry1 rc))
               (< rx1 rxc rx2)) ;if-collide
          (if resolve
              (let ((ryc-new (if (< 0 (- ry1 ryc))
                                 (- ry1 (+ rc resolve))
                                 (+ ry1 (+ rc resolve)))))
                (destructuring-bind (x y)
                    (rotate-points (list rxc ryc-new) rotate)
                  (list x y rotate)))
              t)))))

(defun collision-circle-circle (x1 y1 r1 x2 y2 r2)
  (< (+ (expt (- x2 x1) 2)
	(expt (- y2 y1) 2))
     (expt (+ r1 r2) 2)))

(defun collision-resolve (guy-x guy-y guy-radius obstacles bound-list)
  "pushes circular character (guy-x,guy-y,guy-radius)
away from (obstacles), returns " 
  (let ((poly)(hit-wall)(bound)) 
    (dotimes (poly-index (length obstacles))
      (setq poly (nth poly-index obstacles)
            bound (nth poly-index bound-list))
      (if (< (pythag (- (first bound) guy-x) (- (second bound) guy-y))
             (+ guy-radius (third bound)))
          (dotimes (line-index (1- (/ (length poly) 2))) ;line crosses circle
            (let* ((x1 (nth      (* line-index 2)  poly))
                   (y1 (nth (+ 1 (* line-index 2)) poly))
                   (x2 (nth (+ 2 (* line-index 2)) poly))
                   (y2 (nth (+ 3 (* line-index 2)) poly)))
              (let ((clc-output (collision-line-circle x1 y1 x2 y2 guy-x guy-y guy-radius :resolve 1/1000)))
                (if clc-output 
                    (destructuring-bind (xc yc wall) clc-output
                      (setf hit-wall (if hit-wall
                                         t
                                         wall))
                      (setf guy-x xc
                            guy-y yc)))))))
      (dotimes (point-index (/ (length poly) 2)) ;corner inside circle
        (let ((x (nth     (* 2 point-index)  poly))
              (y (nth (1+ (* 2 point-index)) poly)))
          (if (> guy-radius (pythag (- x guy-x) (- y guy-y)))
              (let ((theta (atan (- guy-y y) (- guy-x x))))
                (setf guy-x (+ x (* guy-radius (cos theta)))
                      guy-y (+ y (* guy-radius (sin theta)))))))))
    (list guy-x guy-y hit-wall)))

(defun generate-bounding-circles (poly-list &aux (bound-list ()))
  "return list of rough bounding circles for each object"
  (dotimes (poly-index (length poly-list))
    (let ((poly (nth poly-index poly-list))
          (x-lo) (x-hi)
          (y-lo) (y-hi))
      (setf x-lo (caar poly)
            x-hi x-lo
            y-lo (cadar poly) ; :/
            y-hi y-lo)
      (do ((point-index 0 (+ 1 point-index))) ((= point-index (length poly)))
	(destructuring-bind (x y) (nth point-index poly)
	  (cond ((< x x-lo) (setf x-lo x))
		((> x x-hi) (setf x-hi x)))
	  (cond ((< y y-lo) (setf y-lo y))
		((> y y-hi) (setf y-hi y)))))
      (let ((x-mid (+ x-lo (/ (- x-hi x-lo) 2)))
	    (y-mid (+ y-lo (/ (- y-hi y-lo) 2)))
	    (r (/ (pythag (- x-hi x-lo) (- y-hi y-lo))
		  2)))
	(push (list x-mid y-mid r) bound-list))))
  (nreverse bound-list))

;; The Top Gameloop
(defun play-a-game (&optional (width 800) (height 800))
  (define-class :fighter 1 20 100)
  (setq *guy* (spawn-mortal :pos '(10 10) :class :fighter))
  (setf *mapbounds* (generate-bounding-circles *map*))
  
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
	   (move *guy* delta-t))
	 
	 (draw-guy *guy*)
	 (draw-map *map* sdl:*magenta*)
	 (sdl:update-display)
	 (sdl:clear-display sdl:*black*))))))