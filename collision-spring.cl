(defun spring (k x &optional (v 0) (c 1))
  "returnes force for a damped spring-mass system"
  (- (* (- k) x)
     (* c v)))

;;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it
(defstruct contact
  depth
  normal				;the direction of the collision
  (thing () :type symbol)		;the thing that hit something
  (thing-pos () :type pt)
  (thing-vel () :type pt)
  (hit () :type symbol)			;the something that thing hit
  hit-pos
  (hit-vel () :type pt))

(declaim (ftype (function (pt pt real) (or null pt-pol)) collision-circle-circle))
;; (defun collision-circle-circle (circle1-pt circle2-pt min-dist)
;;   (declare (optimize speed (safety 0)))
;;   "return the contact if the two points collide"
;;   (let* ((v-diff (v- circle2-pt circle1-pt))
;; 	 (dist (pythag v-diff)))
;;     (when (< dist min-dist)
;;       (make-pt-pol (- min-dist dist) (azimuth v-diff)))))

(defun collision-circle-circle (circle1-pt circle2-pt min-dist) ;this one burns
  (declare (optimize speed (safety 0)))
  "return the contact if the two points collide"
  (let* ((x1 (pt-x circle1-pt))
	 (y1 (pt-y circle1-pt))
	 (x2 (pt-x circle2-pt))
	 (y2 (pt-y circle2-pt))
	 (x (- x2 x1))
	 (y (- y2 y1))
	 (d (sqrt (+ (expt x 2)
		     (expt y 2)))))
    (when (< d min-dist)
      (make-pt-pol (- min-dist d)
		   (atan y x)))))

;; (defun collision-circle-circle-minsqrt (circle1-pt circle2-pt min-dist) ;only sqrt if required, slower for some reason
;;   (let* ((v-diff (v- circle2-pt circle1-pt))
;; 	 (dist-squared (+ (expt (pt-x v-diff) 2)
;; 			  (expt (pt-y v-diff) 2))))
;;     (when (< dist-squared (expt min-dist 2))
;;       (make-pt-pol (- min-dist (sqrt dist-squared))
;; 		   (azimuth v-diff)))))

(defun pt-line-dist (pt line-pt1 line-pt2)
  "return the shortest vector to PT from the line"
  (let* ((line (v- line-pt2 line-pt1))
	 (line-close-pt (v+ line-pt1 (v* (proj pt line-pt2 line-pt1) line))))
    (v- pt line-close-pt)))

(defun pt-seg-dist (pt seg-pt1 seg-pt2)
  "return the shortest vector to PT from the segment"
  (let* ((seg (v- seg-pt2 seg-pt1))
	 (seg-close-pt (v+ seg-pt1 (v* (clamp (proj pt seg-pt2 seg-pt1)) seg))))
    (v- pt seg-close-pt)))

(defun seg-seg-closest (pt-a1 pt-a2 pt-b1 pt-b2)
  "return the shortest vector to line-segment-a from line-segment-b, or NIL if they intersect"
  (let ((seg-a (v- pt-a2 pt-a1))
	(seg-b (v- pt-b2 pt-b1)))
    (cond 
      ((with-slots (x y) seg-a
	 (= 0 x y))
       (pt-seg-dist pt-a1 pt-b1 pt-b2))
      ((with-slots (x y) seg-b
	 (= 0 x y))
       (v* -1 (pt-seg-dist pt-b1 pt-a1 pt-a2)))
      ((= 0 (cross seg-a seg-b))	;parallel
       (let ((dist1 (pt-seg-dist pt-a1 pt-b1 pt-b2))
	     (dist2 (pt-seg-dist pt-a2 pt-b1 pt-b2)))
	 (if (< (pythag dist1) (pythag dist2))
	     dist1
	     dist2)))
      (t
       (let* ((pt1-dist (v- pt-b1 pt-a1))
	      (axb (cross seg-a seg-b))
	      (ahit (/ (cross pt1-dist seg-b) axb))
	      (bhit (/ (cross pt1-dist seg-a) axb)))
	 (if (and (<= 0 ahit 1) (<= 0 bhit 1)) ;if they intersect
	     nil
	     (let* ((seg-a (v- pt-a2 pt-a1))
		    (seg-b (v- pt-b2 pt-b1))
		    (aclose (clamp (cond ((>= bhit 1) (proj pt-b2 pt-a2 pt-a1))
					 ((<= bhit 0) (proj pt-b1 pt-a2 pt-a1))
					 (t ahit))))
		    (bclose (clamp (cond ((>= ahit 1) (proj pt-a2 pt-b2 pt-b1))
					 ((<= ahit 0) (proj pt-a1 pt-b2 pt-b1))
					 (t bhit))))
		    (aclose-pt (v+ pt-a1 (v* aclose seg-a)))
		    (bclose-pt (v+ pt-b1 (v* bclose seg-b))))
	       (v- aclose-pt bclose-pt))))))))

(defun collision-seg-circle (seg-pt1 seg-pt2 circle-pt circle-r)
  "return a contact if the circle collides with the segment"
  (let* ((seg (v- seg-pt2 seg-pt1))	;seg/circ relative forms
	 (circ (v- circle-pt seg-pt1))
	 (col-pt (v* (clamp (proj circ seg)) ;the closest point on the seg to the circle
		     seg)))
    (collision-circle-circle circ col-pt circle-r)))

(defun collision-seg-circle-not-over (seg-pt1 seg-pt2 circle-pt circle-r circle-pt-safe)
  "return a contact if the circle has crossed over the segment since the last safe point"
  (let* ((path-to-seg (seg-seg-closest seg-pt1 seg-pt2 circle-pt circle-pt-safe)))
    (if path-to-seg			;if the seg aren't touching
	;; Then do it normally
	(collision-seg-circle seg-pt1 seg-pt2 circle-pt circle-r)
					;it might be a better plan to
					;take the magnitude of c-l-c
					;with the direction of
					;path-to-seg
	;; Else consider the collision from the other side of the wall
	(let ((r (+ circle-r (pythag (pt-seg-dist circle-pt seg-pt1 seg-pt2))))
	      (theta (+ pi (azimuth (pt-seg-dist circle-pt-safe seg-pt1 seg-pt2)))))
	  (make-pt-pol r theta)))))

(defun collision-seg-seg (pt-a1 pt-a2 pt-b1 pt-b2)
  "returns where along seg A they intersect, in terms of the length of seg A"
  (when (not (= 0 (cross (v- pt-a2 pt-a1) ;don't try if seg are parallel
			 (v- pt-b2 pt-b1)))) ;FIXME: this leaves the remote possibility that someone fires exactly along a wall
    (let* ((pt1-dist (v- pt-b1 pt-a1))
	   (seg-a (v- pt-a2 pt-a1))
	   (seg-b (v- pt-b2 pt-b1))
	   (axb (cross seg-a seg-b))
	   (ahit (/ (cross pt1-dist seg-b) axb))
	   (bhit (/ (cross pt1-dist seg-a) axb)))
      (when (and (< 0 ahit 1)
		 (< 0 bhit 1))
	ahit))))

(defun safe-check (pos0 pos obstacles)
  (catch 'safe
    (dolist (poly obstacles)
      (dotimes (seg-ndx (1- (length poly)))
	(when (collision-seg-seg (nth     seg-ndx  poly)
				 (nth (1+ seg-ndx) poly)
				 pos0 pos)
	  (throw 'safe nil))))
    (throw 'safe t)))

(defun generate-contacts (state0-lst obstacles)
  "return any contacts between 'everyone' and 'obstacles'/eachother"
  ;; (declare (inline collision-circle-circle))
  (let ((contact-lst))
    (dotimes (thing-ndx (length state0-lst))
      (let ((state (elt state0-lst thing-ndx)))
	(with-slots ((thing symbol) pos safe vel) state
	  (let ((shape :circle)		;FIXME: actually get this from somewhere
		(size (attribute thing :size)))
	    ;; collisions with walls
	    (dolist (poly obstacles) ;TODO: only generate one contact if the two collisions are from the same point
	      (dotimes (seg-ndx (1- (length poly)))
		(case shape
		  (:circle (let ((seg-acontact (collision-seg-circle-not-over
						(nth     seg-ndx  poly)
						(nth (1+ seg-ndx) poly)
						pos size safe
						)))
			     (when seg-acontact
			       (let ((depth (pt-pol-r seg-acontact))
				     (normal (pt-pol-theta seg-acontact)))
					;(setf *debug-contact* seg-acontact) ;TEST
				 (push (make-contact :depth depth
						     :normal normal
						     :thing thing
						     :thing-pos pos
						     :thing-vel vel
						     :hit :wall
						     :hit-vel (make-pt))
				       contact-lst)))))
		  )))
	    ;; collisions with other guys, do each pair once
	    (dotimes (o-thing-ndx thing-ndx)
	      (let* ((other-state (elt state0-lst o-thing-ndx)))
		(with-slots ((o-thing symbol) (o-pos pos) (o-vel vel)) other-state
		  (let* ((o-size (attribute o-thing :size))
			 (melee-acontact (collision-circle-circle pos o-pos (+ size o-size))))
		    (when melee-acontact
		      (let ((depth (pt-pol-r melee-acontact))
			    (normal (pt-pol-theta melee-acontact)))
			(push (make-contact :depth depth
					    :normal normal
					    :thing thing
					    :thing-pos pos
					    :thing-vel vel
					    :hit o-thing
					    :hit-pos o-pos
					    :hit-vel o-vel)
			      contact-lst)))))))
	    ;; particle collisions go here
	    ))))
    contact-lst))

(defun collision-resolve (state0-lst obstacles)
  "returns the forces from collisions"
  (let ((contact-lst (generate-contacts state0-lst obstacles))
	(forces-plist))
    (dolist (contact contact-lst)
      (with-slots (depth normal thing thing-vel hit hit-vel) contact
	;; calculate a force for each contact
	(let* ((vel-diff (v- thing-vel
			     (or hit-vel (make-pt))))
	       (vel-diff-component (component vel-diff normal))
	       ;; this is where I make different surfaces 'feel' different to smack into
	       (force-r (let* ((thing-type :guy)
			       (hit-type (if (eq hit :wall)
					     :wall
					     :guy)))
			  (destructuring-bind (k c) (getf (getf *collision-flavor* thing-type) hit-type)
			    (spring k depth vel-diff-component c)))))
	  ;; Displacement depth is positive, so the spring force ends
	  ;; up being negative, and we end up not having to reverse
	  ;; the contact normal before using it again!  Neat, huh?
	  (push (carterize (make-pt-pol force-r normal))
		(getf forces-plist thing))
	  (when hit-vel			;every action is coupled with an equal and opposite reaction
	    (push (carterize (make-pt-pol force-r (+ pi normal)))
		  (getf forces-plist hit))))))
    ;; collect forces into one aggregate force
    (when forces-plist
      (do ((i 0 (+ 2 i)))		  ;FIXME: I don't like this being here
	  ((> i (1- (length forces-plist))))
	(let ((forces (nth (1+ i) forces-plist)))
	  (setf (nth (1+ i) forces-plist)
		(apply 'v+ forces)))))
    forces-plist))
