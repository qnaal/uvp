;contact: (depth normal guy1 [guy2|:wall])
;acontact: (depth normal)
;normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it

(defun collision-line-circle (line-pt1 line-pt2 circle-pt circle-r)
  "returns contact if applicable"
  (let ((line-angle (azimuth (v- line-pt2 line-pt1))))
    (destructuring-bind ((rx1 ry1) (rx2 ry2) (rxc ryc))
	(rotate-points (- line-angle)
		       line-pt1 line-pt2 circle-pt)
      (declare (ignore ry2))
      (let ((distance-from-parallel (abs (- ryc ry1)))
	    )
	(if (and (< distance-from-parallel circle-r)
		 (< rx1 rxc rx2))
	    (list (- circle-r distance-from-parallel)
		  (- line-angle (* (if (plusp (- ryc ry1))
				       1
				       -1)
				   (/ pi 2)))
		  ))))))

(defun collision-circle-circle (circle1-pt circle2-pt min-dist)
  (let* ((v-diff (v- circle2-pt circle1-pt))
	 (dist (abs (pythag v-diff))))
    (if (< dist min-dist)
	(list (- min-dist dist) (azimuth v-diff)))))
	

(defun generate-contacts (everyone pos-lst obstacles)
  (let ((contact-lst))
    (dolist (poly obstacles)
      (dotimes (line-ndx (1- (length poly)))
	(dotimes (guy-ndx (length everyone))
	  (let* ((guy (nth guy-ndx everyone))
		 (pos (nth guy-ndx pos-lst))
		 (size (attribute guy :size))
		 (clc-output (collision-line-circle (nth     line-ndx  poly)
						    (nth (1+ line-ndx) poly)
						    pos size
						    )))
	    (if clc-output
		(destructuring-bind (depth normal)
		    clc-output
		  ;(setf *debug-contact* clc-output) ;TEST
		  (push (list depth normal guy) contact-lst)))))))
    contact-lst))


;;outputs a plist:
;(guy (force [otherguy|:wall]) ...)
(defun collision-resolve (everyone pos-lst obstacles)
  "returns a plist of the required force for every guy"
  (let ((contact-lst (generate-contacts everyone pos-lst obstacles))
	;(force-lst-plst) ;plist of every collision force, for each guy
	(guy-force-lst)
	(force-plst)) ;plist of the combined collision force, for each guy
    (dolist (contact contact-lst)
      (destructuring-bind (depth normal guy) contact
	(push (list guy (carterize (list (spring 5000 depth) normal))) guy-force-lst)))
    (dolist (guy-force guy-force-lst)
      (destructuring-bind (guy force)
	  guy-force
	(let ((oldforce (getf force-plst guy)))
	  (if oldforce
	      (setf (getf force-plst guy) (v+ force oldforce))
	      (setf (getf force-plst guy) force)))))
    (if (getf force-plst *guy*)		;TEST
	(setf *debug-contact* (getf force-plst *guy*)))
    force-plst))