
(defun update-velocity (guy input delta-t)
  (let* ((ir (car input))
	 (itheta (cadr input))
	 (vel (attribute guy :vel-pol))
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

(defun move (pos vel delta-t)
  (destructuring-bind (vr vtheta) vel
    (destructuring-bind (x y) pos
      (let ((vx (* vr (cos vtheta)))
	    (vy (* vr (sin vtheta))))
	(list (+ x (* vx delta-t))
	      (+ y (* vy delta-t)))))))

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
      (declare (ignore ry2))
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

(defun collision-circle-circle (x1 y1 r1 x2 y2 &optional (r2 0))
  (< (+ (expt (- x2 x1) 2)
	(expt (- y2 y1) 2))
     (expt (+ r1 r2) 2)))

(defun collision-resolve-guy-wall (guy-x guy-y guy-radius obstacles bound-list)
  "pushes circular character (guy-x,guy-y,guy-radius)
away from (obstacles) he is touching, returns new position" 
  (let ((poly)(hit-wall)(bound)(hit)) 
    (dotimes (poly-index (length obstacles))
      (setq poly (nth poly-index obstacles)
            bound (nth poly-index bound-list))
      (if (< (pythag (- (first bound) guy-x) (- (second bound) guy-y))
             (+ guy-radius (third bound)))
	  ;;line crosses circle
	  ;;FIXME: gets jittery in concave corners;
	  ;;should find where lines cross and put Guy there
          (dotimes (line-index (1- (length poly))) ;line crosses circle
	    (destructuring-bind
		  ((x1 y1) (x2 y2))
		(list (nth     line-index  poly)
		      (nth (1+ line-index) poly))
	      (let ((clc-output (collision-line-circle x1 y1 x2 y2 guy-x guy-y guy-radius :resolve *buffer*)))
		(if clc-output 
		    (destructuring-bind (xc yc wall) clc-output
		      (setf hit-wall (if hit-wall
					 t
					 wall)
			    guy-x xc
			    guy-y yc
			    line-index 0
			    hit t)))))))
      (dotimes (point-index (length poly)) ;corner inside circle
	(destructuring-bind (x y) (nth point-index poly)
          (if (> guy-radius (pythag (- x guy-x) (- y guy-y)))
              (let ((theta (atan (- guy-y y) (- guy-x x))))
                (setf guy-x (+ x (* guy-radius (cos theta)))
                      guy-y (+ y (* guy-radius (sin theta)))
		      hit t))))))
    (if hit
	(list (list guy-x guy-y) hit-wall))))

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

(defun collision-resolve (everyone)
  ;;cleared guys aren't touching any other cleared guys, or any walls
  (let* ((cleared (make-list (length everyone))))
    (do* ((guy-ndx 0 (mod (1+ guy-ndx)
			  (length everyone)))
	  (guy (car everyone) (nth guy-ndx everyone)))
	 ;;If everyone's clear, we're good. Is there a better way to do this?
	 ((let ((lst cleared))
	    (dotimes (x (length lst) t)
	      (if (null (nth x lst))
		  (return)))))
      (setf (nth guy-ndx cleared) t)
					;(print cleared)
      
      (destructuring-bind ((x y) r loop-end) (list (attribute guy :pos)
						   (attribute guy :size)
						   0)
	(do* ((other-guy-ndx 0 (mod (1+ other-guy-ndx)
				    (length everyone)))
	      (super-retarded-hack-switch nil t)
	      (other-guy (car everyone) (nth other-guy-ndx everyone)))
	     ((and (= other-guy-ndx loop-end)
		   super-retarded-hack-switch))

	  (when (not (eq guy other-guy))
	    (destructuring-bind ((x2 y2) r2)
		(list (attribute other-guy :pos)
		      (attribute other-guy :size))
	      (when (collision-circle-circle x y r x2 y2 r2)
		;; We're moving him, so we can't trust that he's not hitting anything anymore
		(setf (nth other-guy-ndx cleared) nil
		      ;; also restart the loop
		      loop-end other-guy-ndx)
		(let* ((xdiff (- x2 x))
		       (ydiff (- y2 y))
		       (theta (atan ydiff
				    xdiff))
		       (cos-theta (cos theta))
		       (sin-theta (sin theta))
		       
		       (overlap (- (+ r r2)
				   (pythag xdiff ydiff)))
		       (pushdistance (+ overlap *buffer*))
		       (weight (attribute guy :weight))
		       (weight2 (attribute other-guy :weight))
		       (push (/ weight
				(+ weight weight2))))
		  (setf x  (+ x (* (- 1 push)
				   pushdistance
				   (- cos-theta)))
			y  (+ y (* (- 1 push)
				   pushdistance
				   (- sin-theta)))
			x2 (+ x2 (* push
				    pushdistance
				    cos-theta))
			y2 (+ y2 (* push
				    pushdistance
				    sin-theta)))
		  (setf (get other-guy :pos) (list x2 y2)))))))
	(bottleneck 'col-guy)
	(let ((collision (collision-resolve-guy-wall x y r
						     *map*
						     *mapbounds*)))
	  (when collision
	    (destructuring-bind ((x-new y-new) hit-wall) collision
	      (declare (ignore hit-wall))
	      (setf x x-new
		    y y-new))))
	(setf (get guy :pos) (list x y))
	(bottleneck 'col-wall)
	))))