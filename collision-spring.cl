;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it

(defstruct contact
  depth
  normal				;the direction of the collision
  (thing () :type symbol)		;the thing that hit something
  (thing-pos () :type pt)
  (thing-vel () :type pt)
  (hit () :type symbol)			;the something that thing hit
  hit-pos
  (hit-vel () :type pt))

(defun collision-line-circle (line-pt1 line-pt2 circle-pt circle-r)
  "return a contact if the circle collides with the line"
  (let* ((line (v- line-pt2 line-pt1))	;line/circ relative forms
	 (circ (v- circle-pt line-pt1))
	 (col-pt (v* (clamp (proj circ line) 0 1) ;the closest point on the line to the circle
		     line)))
    (collision-circle-circle circ col-pt circle-r)))
		   
(declaim (ftype (function (pt pt real) (or null pt-pol)) collision-circle-circle))
;; (defun collision-circle-circle (circle1-pt circle2-pt min-dist)
;;   (declare (optimize speed (safety 0)))
;;   "return the contact if the two points collide"
;;   (let* ((v-diff (v- circle2-pt circle1-pt))
;; 	 (dist (pythag v-diff)))
;;     (when (< dist min-dist)
;;       (make-pt-pol (- min-dist dist) (azimuth v-diff)))))

(defun collision-circle-circle (circle1-pt circle2-pt min-dist) ;this one burns
  "return the contact if the two points collide"
  (declare (optimize speed (safety 0)))
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

(defun collision-line-line (pt-a1 pt-a2 pt-b1 pt-b2) ;haven't tested this yet, it should work
  "returns where along line A they intersect, in terms of the length of line A"
  (when (not (= 0 (cross (v- pt-a2 pt-a1) ;don't try if lines are parallel
			 (v- pt-b2 pt-b1)))) ;FIXME: this leaves the remote possibility that someone fires exactly along a wall
    (let ((ahit (/ (cross (v- pt-b1 pt-a1)
			  (v- pt-a2 pt-a1))
		   (cross (v- pt-a2 pt-a1)    ;same
			  (v- pt-b2 pt-b1)))) ;
	  (bhit (/ (cross (v- pt-b1 pt-a1)
			  (v- pt-b2 pt-b1))
		   (cross (v- pt-a2 pt-a1)     ;same
			  (v- pt-b2 pt-b1))))) ;
      (when (and (< 0 ahit 1)
		 (< 0 bhit 1))
	ahit))))


(defun generate-contacts (state0-lst obstacles)
  "return any contacts between 'everyone' and 'obstacles'/eachother"
  ;; (declare (inline collision-circle-circle))
  (let ((contact-lst))
    (dotimes (thing-ndx (length state0-lst))
      (let ((state (elt state0-lst thing-ndx)))
	(with-slots ((thing symbol) pos vel) state
	  (let ((size (attribute thing :size)))
	    ;; collisions with walls
	    (dolist (poly obstacles) ;TODO: only generate one contact if the two collisions are from the same point
	      (dotimes (line-ndx (1- (length poly)))
		(let ((line-acontact (collision-line-circle (nth     line-ndx  poly)
							    (nth (1+ line-ndx) poly)
							    pos size
							    )))
		  (when line-acontact
		    (let ((depth (pt-pol-r line-acontact))
			  (normal (pt-pol-theta line-acontact)))
					;(setf *debug-contact* line-acontact) ;TEST
		      (push (make-contact :depth depth
					  :normal normal
					  :thing thing
					  :thing-pos pos
					  :thing-vel vel
					  :hit :wall
					  :hit-vel (make-pt))
			    contact-lst))))))
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
