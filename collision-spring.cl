;contact: (depth normal guy1 [guy2|:wall])
;acontact: (depth normal) "anonymous contact"
;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it
;; FIXME: the above information might not actually be completely correct

;; (defstruct contact
;;   vec-pol				;contact vector
;;   thing					;the thing that hit something
;;   hit					;the something that thing hit
;;   )
  

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

;; (defun collision-circle-circle (circle1-pt circle2-pt min-dist) ;only sqrt if required, slower for some reason
;;   (let* ((v-diff (v- circle2-pt circle1-pt))
;; 	 (dist-squared (+ (expt (pt-x v-diff) 2)
;; 			  (expt (pt-y v-diff) 2))))
;;     (when (< dist-squared (expt min-dist 2))
;;       (make-pt-pol (- min-dist (sqrt dist-squared))
;; 		   (azimuth v-diff)))))


(defun generate-contacts (everyone pos-lst obstacles)
  "return any contacts between 'everyone' and 'obstacles'/eachother"
  ;; (declare (inline collision-circle-circle))
  (let ((contact-lst))
    (dotimes (guy-ndx (length everyone))
      (let* ((guy (nth guy-ndx everyone))
	     (pos (nth guy-ndx pos-lst))
	     (size (attribute guy :size)))
	;; collisions with walls
	(dolist (poly obstacles)	;FIXME: only generate one contact if the two collisions are from the same point
	  (dotimes (line-ndx (1- (length poly)))
	    (let ((line-acontact (collision-line-circle (nth     line-ndx  poly)
							(nth (1+ line-ndx) poly)
							pos size
							)))
	      (when line-acontact
		(let ((depth (pt-pol-r line-acontact))
		      (normal (pt-pol-theta line-acontact)))
					;(setf *debug-contact* line-acontact) ;TEST
		  (push (list depth normal guy :wall) contact-lst))))))
	;; collisions with other guys, do each pair once
	(dotimes (o-guy-ndx guy-ndx)
	  (let* ((other-guy (nth o-guy-ndx everyone))
		 (o-pos (nth o-guy-ndx pos-lst))
		 (o-size (attribute other-guy :size))
		 (melee-acontact (collision-circle-circle pos o-pos (+ size o-size))))
	    (when melee-acontact
	      (let ((depth (pt-pol-r melee-acontact))
		    (normal (pt-pol-theta melee-acontact)))
		(push (list depth (+ pi normal) other-guy guy) contact-lst)
		(push (list depth normal guy other-guy) contact-lst)))))
	))
    contact-lst))


;;outputs a plist:
;(guy force ...)
(defun collision-resolve (everyone pos-lst vel-lst obstacles) ;FIXME: retype this piece of shit
  "returns a plist of the required force for every guy"
  (let ((contact-lst (generate-contacts everyone pos-lst obstacles))
	(vel-plst (mapcan 'list everyone vel-lst))
	(guy-force-lst)
	(force-plst)) ;plist of the combined collision force, for each guy

    (dolist (contact contact-lst)
      (destructuring-bind (depth normal guy thing) contact
	;; calculate a force for each contact
	(let* ((vel-diff (if (getf vel-plst thing)
	       		     (v- (getf vel-plst guy) (getf vel-plst thing))
	       		     (getf vel-plst guy)))
	       (vel-diff-component (component vel-diff normal))

	       ;; this is where I make different surfaces 'feel' different to smack into
	       (force-r (let* ((guy-type :guy)
	       		       (thing-type (if (eq thing :wall)
	       				       :wall
	       				       :guy)))
	       		  (destructuring-bind (k c) (getf (getf *collision-flavor* guy-type) thing-type)
	       		    (spring k depth vel-diff-component c))))
	       )
	  ;; Displacement depth is positive, so the spring force ends
	  ;; up being negative, and we end up not having to reverse
	  ;; the contact normal before using it again!  Neat, huh?
	  (push (list guy (carterize (make-pt-pol force-r normal))) guy-force-lst))))
    
    ;; collect forces into one aggregate force for each guy
    (dolist (guy-force guy-force-lst)
      (destructuring-bind (guy force) guy-force
	(let ((oldforce (getf force-plst guy)))
	  (if oldforce
	      (setf (getf force-plst guy) (v+ force oldforce))
	      (setf (getf force-plst guy) force)))))
    ;; (if (getf force-plst *guy*)		;TEST
    ;; 	(setf *debug-contact* (getf force-plst *guy*)))

    ;; FIXME: return something other than a plist
    force-plst))
