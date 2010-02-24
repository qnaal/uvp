;contact: (depth normal guy1 [guy2|:wall])
;acontact: (depth normal) "anonymous contact"
;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it

;; (defun collision-line-circle (line-pt1 line-pt2 circle-pt circle-r)
;;   "returns contact if applicable"
;;   (let ((line-angle (azimuth (v- line-pt2 line-pt1))))
;;     (destructuring-bind ((rx1 ry1) (rx2 ry2) (rxc ryc))
;; 	(rotate-points (- line-angle)
;; 		       line-pt1 line-pt2 circle-pt)
;;       (declare (ignore ry2))
;;       (let ((distance-from-parallel (abs (- ryc ry1)))
;; 	    )
;; 	(if (and (< distance-from-parallel circle-r)
;; 		 (< rx1 rxc rx2))
;; 	    (list (- circle-r distance-from-parallel)
;; 		  (- line-angle (* (if (plusp (- ryc ry1))
;; 				       1
;; 				       -1)
;; 				   (/ pi 2)))))))))

(defun collision-line-circle (line-pt1 line-pt2 circle-pt circle-r)
  (let* ((line (v- line-pt2 line-pt1))	;line/circ relative forms
	 (circ (v- circle-pt line-pt1))
	 (col-pt (v* (clamp (proj circ line) 0 1) ;the closest point on the line to the circle
		     line)))
    (collision-circle-circle circ col-pt circle-r)))
		     

(defun collision-circle-circle (circle1-pt circle2-pt min-dist)
;;  (declare (inline v- pythag azimuth))
  (let* ((v-diff (v- circle2-pt circle1-pt))
	 (dist (pythag v-diff)))
    (when (< dist min-dist)
      (list (- min-dist dist) (azimuth v-diff)))))

;; (defun collision-circle-circle (circle1-pt circle2-pt min-dist) ;inline
;;   (let* ((v-diff (mapcar '- circle2-pt circle1-pt))
;;          (dist (destructuring-bind (x y) v-diff
;;                  (sqrt (+ (expt x 2)
;;                           (expt y 2))))))
;;     (if (< dist min-dist)
;;         (list (- min-dist dist) (destructuring-bind (x y) v-diff
;;                                   (atan y x))))))

;; (defun collision-circle-circle (circle1-pt circle2-pt min-dist) ;only sqrt if required, slower for some reason
;;   (let* ((v-diff (v- circle2-pt circle1-pt))
;; 	 (dist-squared (apply '+ (mapcar '* v-diff v-diff))))
;;     (if (< dist-squared (expt min-dist 2))
;; 	(list (- min-dist (expt dist-squared 1/2))
;; 	      (azimuth v-diff)))))


(defun generate-contacts (everyone pos-lst obstacles)
  (declare (inline collision-circle-circle))
  (let ((contact-lst))
    (dotimes (guy-ndx (length everyone))
      (let* ((guy (nth guy-ndx everyone))
	     (pos (nth guy-ndx pos-lst))
	     (size (attribute guy :size)))

	(dolist (poly obstacles)	;collisions with walls
	  (dotimes (line-ndx (1- (length poly)))
	    (let ((line-acontact (collision-line-circle (nth     line-ndx  poly)
							(nth (1+ line-ndx) poly)
							pos size
							)))
	      (if line-acontact
		  (destructuring-bind (depth normal)
		      line-acontact
					;(setf *debug-contact* line-acontact) ;TEST
		    (push (list depth normal guy :wall) contact-lst)))))
	  (dolist (corner poly)
	    (let ((corner-acontact (collision-circle-circle pos corner size)))
	      (if corner-acontact
		  (destructuring-bind (depth normal)
		      corner-acontact
		    (push (list depth normal guy :wall) contact-lst))))))

	;; (dotimes (o-guy-ndx (length everyone)) ;collisions with other guys
	;;   (if (/= guy-ndx o-guy-ndx)
	;;       (let* ((other-guy (nth o-guy-ndx everyone))
	;; 	     (o-pos (nth o-guy-ndx pos-lst))
	;; 	     (o-size (attribute other-guy :size))
	;; 	     (melee-acontact (collision-circle-circle pos o-pos (+ size o-size))))
	;; 	(if melee-acontact
	;; 	  (destructuring-bind (depth normal)
	;; 	      melee-acontact
	;; 	    (push (list depth normal guy other-guy) contact-lst))))))
	(dotimes (o-guy-ndx guy-ndx) ;collisions with other guys
	  (let* ((other-guy (nth o-guy-ndx everyone))
		 (o-pos (nth o-guy-ndx pos-lst))
		 (o-size (attribute other-guy :size))
		 (melee-acontact (collision-circle-circle pos o-pos (+ size o-size))))
	    (if melee-acontact
		(destructuring-bind (depth normal)
		    melee-acontact
		  (push (list depth (+ pi normal) other-guy guy) contact-lst)
		  (push (list depth normal guy other-guy) contact-lst)))))
	))
    contact-lst))


;;outputs a plist:
;(guy force ...)
(defun collision-resolve (everyone pos-lst vel-lst obstacles)
  "returns a plist of the required force for every guy"
  (let ((contact-lst (generate-contacts everyone pos-lst obstacles))
	(vel-plst (mapcan 'list everyone vel-lst))
	(guy-force-lst)
	(force-plst)) ;plist of the combined collision force, for each guy
    (dolist (contact contact-lst)
      (destructuring-bind (depth normal guy thing) contact
	(let* ((vel-diff (if (getf vel-plst thing)
			     (v- (getf vel-plst guy) (getf vel-plst thing))
			     (getf vel-plst guy)))
	       (vel-diff-component (component (polarize vel-diff) normal))
	       (force-r (let* ((guy-type :guy)
			       (thing-type (if (eq thing :wall)
					       :wall
					       :guy)))
			  (destructuring-bind (k c) (getf (getf *collision-flavor* guy-type) thing-type)
			    (spring k depth vel-diff-component c)))))
	  (push (list guy (carterize (list force-r normal))) guy-force-lst))))
    (dolist (guy-force guy-force-lst)
      (destructuring-bind (guy force)
	  guy-force
	(let ((oldforce (getf force-plst guy)))
	  (if oldforce
	      (setf (getf force-plst guy) (v+ force oldforce))
	      (setf (getf force-plst guy) force)))))
    ;; (if (getf force-plst *guy*)		;TEST
    ;; 	(setf *debug-contact* (getf force-plst *guy*)))
    force-plst))
