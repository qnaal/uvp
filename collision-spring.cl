;contact: (depth normal guy1 [guy2|:wall])
;acontact: (depth normal) "anonymous contact"
;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it
;; FIXME: the above information might not actually be completely correct

(defstruct contact
  depth
  normal				;the direction of the collision
  thing					;the thing that hit something
  hit					;the something that thing hit
  )
  

(defun collision-line-circle (line-pt1 line-pt2 circle-pt circle-r)
  "return a contact if the circle collides with the line"
  (let* ((line (v- line-pt2 line-pt1))	;line/circ relative forms
	 (circ (v- circle-pt line-pt1))
	 (col-pt (v* (clamp (proj circ line) 0 1) ;the closest point on the line to the circle
		     line)))
    (collision-circle-circle circ col-pt circle-r)))
		     
(defun collision-circle-circle (circle1-pt circle2-pt min-dist)
  "return the contact if the two points collide"
  (let* ((v-diff (v- circle2-pt circle1-pt))
	 (dist (pythag v-diff)))
    (when (< dist min-dist)
      (make-pt-pol (- min-dist dist) (azimuth v-diff)))))

;; (defun collision-circle-circle-burns (circle1-pt circle2-pt min-dist) ;this one burns
;;   "return the contact if the two points collide"
;;   (let* ((x1 (pt-x circle1-pt))
;; 	 (y1 (pt-y circle1-pt))
;; 	 (x2 (pt-x circle2-pt))
;; 	 (y2 (pt-y circle2-pt))
;; 	 (x (- x2 x1))
;; 	 (y (- y2 y1))
;; 	 (d (sqrt (+ (expt x 2)
;; 		     (expt y 2)))))
;;     (when (< d min-dist)
;;       (make-pt-pol (- min-dist d)
;; 		   (atan y x)))))


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

(defun generate-contacts (everyone state-indicator obstacles)
  "return any contacts between 'everyone' and 'obstacles'/eachother"
  ;; (declare (inline collision-circle-circle))
  (let ((contact-lst))
    (dotimes (guy-ndx (length everyone)) ;if I do this with 'do' I won't have to count up to 'guy'
      (let* ((guy (nth guy-ndx everyone))
	     (pos (state-pos (get guy state-indicator)))
	     (size (attribute guy :size)))
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
				      :thing guy
				      :hit :wall)
			contact-lst))))))
	;; collisions with other guys, do each pair once
	(dotimes (o-guy-ndx guy-ndx)
	  (let* ((other-guy (nth o-guy-ndx everyone))
		 (o-pos (state-pos (get other-guy state-indicator)))
		 (o-size (attribute other-guy :size))
		 (melee-acontact (collision-circle-circle pos o-pos (+ size o-size))))
	    (when melee-acontact
	      (let ((depth (pt-pol-r melee-acontact))
		    (normal (pt-pol-theta melee-acontact)))
		(push (make-contact :depth depth
				    :normal normal
				    :thing guy
				    :hit other-guy)
		      contact-lst)
		(push (make-contact :depth depth
				    :normal (+ pi normal)
				    :thing other-guy
				    :hit guy)
		      contact-lst)))))
	;; particle collisions go here
	))
    contact-lst))


;;outputs a plist:
;(guy force ...)
(defun collision-resolve (everyone state-indicator obstacles)
  "returns a plist of the required force for every guy"
  (let ((contact-lst (generate-contacts everyone state-indicator obstacles))
	(guy-force-lst)
	(force-plst)) ;plist of the combined collision force, for each guy

    (dolist (contact contact-lst)
      (let ((depth (contact-depth contact))
	    (normal (contact-normal contact))
	    (thing (contact-thing contact))
	    (hit (contact-hit contact)))
	;; calculate a force for each contact
	(let* ((vel-diff (if (get hit state-indicator) ;TODO: do this less dumb
	       		     (v- (state-vel (get thing state-indicator))
				 (state-vel (get hit state-indicator)))
	       		     (state-vel (get thing state-indicator))))
	       (vel-diff-component (component vel-diff normal))

	       ;; this is where I make different surfaces 'feel' different to smack into
	       (force-r (let* ((thing-type :guy)
	       		       (hit-type (if (eq hit :wall)
	       				       :wall
	       				       :guy)))
	       		  (destructuring-bind (k c) (getf (getf *collision-flavor* thing-type) hit-type)
	       		    (spring k depth vel-diff-component c))))
	       )
	  ;; Displacement depth is positive, so the spring force ends
	  ;; up being negative, and we end up not having to reverse
	  ;; the contact normal before using it again!  Neat, huh?
	  (push (list thing (carterize (make-pt-pol force-r normal))) guy-force-lst))))
    
    ;; collect forces into one aggregate force for each guy
    (dolist (guy-force guy-force-lst)
      (destructuring-bind (guy force) guy-force
	(let ((oldforce (getf force-plst guy)))
	  (if oldforce
	      (setf (getf force-plst guy) (v+ force oldforce))
	      (setf (getf force-plst guy) force)))))
    ;; (if (getf force-plst *guy*)		;TEST
    ;; 	(setf *debug-contact* (getf force-plst *guy*)))

    ;; TODO: return something other than a plist
    force-plst))
