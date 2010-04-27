(defun spring (k x &optional (v 0) (c 1))
  "returnes force for a damped spring-mass system"
  (- (* (- k) x)
     (* c v)))

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
  (when (v= line-pt1 line-pt2)
    (error "need two points to define a line"))
  (let* ((line (v- line-pt2 line-pt1))
	 (line-close-pt (v+ line-pt1 (v* (proj pt line-pt2 line-pt1) line))))
    (v- pt line-close-pt)))

(defun pt-seg-dist (pt seg-pt1 seg-pt2)
  "return the shortest vector to PT from the segment"
  (let ((seg (v- seg-pt2 seg-pt1)))
    (if (v= seg (make-pt-board))
	(v- pt seg-pt1)
	(let ((seg-close-pt (v+ seg-pt1 (v* (clamp (proj pt seg-pt2 seg-pt1)) seg))))
	  (v- pt seg-close-pt)))))

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

(defun collision-circle-seg (circle-pt circle-r seg-pt1 seg-pt2)
  "return a contact if the circle collides with the segment"
  (let* ((seg (v- seg-pt2 seg-pt1))	;seg/circ relative forms
	 (circ (v- circle-pt seg-pt1)))	;
    (if (v= seg (make-pt-board))
	(collision-circle-circle circle-pt seg-pt1 circle-r)
	(let ((col-pt (v* (clamp (proj circ seg)) ;the closest point on the seg to the circle
			  seg)))
	  (collision-circle-circle circ col-pt circle-r)))))

(defun collision-circle-seg-not-over (circle-pt circle-r circle-pt-safe seg-pt1 seg-pt2)
  "return a contact if the circle has crossed over the segment since the last safe point"
  (let* ((path-to-seg (seg-seg-closest seg-pt1 seg-pt2 circle-pt circle-pt-safe)))
    (if path-to-seg			;if the seg aren't touching
	;; Then do it normally
	(collision-circle-seg circle-pt circle-r seg-pt1 seg-pt2)
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

(defun chord-slice (circle-pt circle-r line-pt1 line-pt2)
  "returns the chord of CIRCLE that is a segment of LINE"
  (let* ((pt-line (pt-line-dist circle-pt line-pt1 line-pt2))
	 (pt-line-r (pythag pt-line)))
    (when (> circle-r pt-line-r)
      (let* ((length (* 2 (sqrt (- (expt circle-r 2)
				   (expt pt-line-r 2)))))
	     (theta (azimuth (v- line-pt2 line-pt1)))
	     (chord-pol (make-pt-pol length theta)))
	(carterize chord-pol)))))

(defun collision-circle-path (circ-pt circ-r path-pt1 path-pt2 &optional (path-r 0))
  "fudges a believable contact between CIRC and a point following PATH"
  (let* ((total-r (+ circ-r path-r))
	 (pt-dist (pt-seg-dist circ-pt path-pt1 path-pt2))
	 (pt-dist-r (pythag pt-dist)))
    (cond
      ((> pt-dist-r total-r)		;they don't touch
       nil)
      ((v= path-pt1 path-pt2)		;path is a point
       (collision-circle-circle circ-pt path-pt1 total-r))
      (t
       (let* ((chord (chord-slice circ-pt circ-r path-pt1 path-pt2))
	      (chord-start (v+ pt-dist (v* -1/2 chord))) ;relative to circ-pt
	      (theta (azimuth chord-start))
	      (acontact (make-pt-pol pt-dist-r theta)))
	 acontact)))))

(defstruct shape
  )

(defstruct (circle (:include shape))
  r)

(defstruct (circle-sweep (:include circle))) ;setting this dynamically on fast objects would be pretty cool...

(defstruct (segment (:include shape))
  pt1 pt2)

(macrolet (
	   (with-circle ((pos r safe) state &body body)
	     (let ((shape (gensym))
		   (symbol (gensym)))
	       `(with-slots ((,pos pos) (,symbol symbol) (,safe safe)) ,state
		  (let ((,shape (attribute ,symbol :shape)))
		    (let ((,r (gur-to-board (circle-r ,shape))))
		      ,@body)))))
	   (with-circle-sweep ((pos r safe &optional ppos) state &body body)
	     (let ((shape (gensym))
		   (symbol (gensym)))
	       `(with-slots ((,pos pos) (,symbol symbol) (,safe safe)) ,state
		  (let ((,shape (attribute ,symbol :shape))
			,@(when ppos
				`((,ppos (attribute ,symbol :pos))) ;FIXME: ppos needs to be grabbed from STATE
				))
		    (let ((,r (gur-to-board (circle-r ,shape))))
		      ,@body)))))
	   (with-segment ((pt1-abs pt2-abs) state &body body)
	     (let ((shape (gensym))
		   (symbol (gensym))
		   (pos (gensym))
		   (pt1 (gensym))
		   (pt2 (gensym)))
	       `(with-slots ((,pos pos) (,symbol symbol)) ,state
		  (let ((,shape (attribute ,symbol :shape)))
		    (with-slots ((,pt1 pt1) (,pt2 pt2)) ,shape
		      (let ((,pt1-abs (v+ ,pt1 ,pos))
			    (,pt2-abs (v+ ,pt2 ,pos)))
			,@body))))))
	   )
  (defun collision-generic (obj1-state obj2-state)
    (let* (
	   (earlier-shape-name) (later-shape-name)
	   (earlier-obj) (later-obj)
	   (flip)
	   (shape-lst '(circle circle-sweep segment))
	   (obj1 (state-symbol obj1-state))
	   (obj2 (state-symbol obj2-state))
	   (obj1-shape (attribute obj1 :shape))
	   (obj2-shape (attribute obj2 :shape))
	   (obj1-shape-name (type-of obj1-shape))
	   (obj2-shape-name (type-of obj2-shape))
	   (pos1 (position obj1-shape-name shape-lst))
	   (pos2 (position obj2-shape-name shape-lst))
	   )
      (if (<= pos1 pos2)
	  (setf earlier-obj obj1-state
		earlier-shape-name obj1-shape-name
		later-obj obj2-state
		later-shape-name obj2-shape-name
		)
	  (setf earlier-obj obj2-state
		earlier-shape-name obj2-shape-name
		later-obj obj1-state
		later-shape-name obj1-shape-name
		flip t
		))
      (let ((returned-acontact
	     (case earlier-shape-name
	       (circle
		(with-circle
		    (e-pos e-r e-safe) earlier-obj
		    (case later-shape-name
		      (circle
		       (with-circle
			   (l-pos l-r l-safe) later-obj
			   (collision-circle-circle e-pos l-pos (+ e-r l-r))
			   )
		       )
		      (circle-sweep
		       (with-circle-sweep
			   (l-pos l-r l-safe l-ppos) later-obj
			   (collision-circle-path e-pos e-r l-pos l-ppos)))
		      (segment
		       (with-segment
			   (l-pt1 l-pt2) later-obj
			   (collision-circle-seg-not-over e-pos e-r e-safe l-pt1 l-pt2)
			   )
		       ))))
	       (circle-sweep
		(with-circle-sweep
		    (e-pos e-r e-safe) earlier-obj
		    (case later-shape-name
		      (circle-sweep
		       (with-circle-sweep
			   (l-pos l-r l-safe) later-obj
			   (collision-circle-circle e-pos l-pos (+ e-r l-r))))
		      (segment
		       (with-segment
			   (l-pt1 l-pt2) later-obj
			   (collision-circle-seg-not-over e-pos e-r e-safe l-pt1 l-pt2))))))
	       (segment
		(case later-shape-name
		  (segment
		   ))))))
	(if returned-acontact
	    (if flip
		(invert-pol returned-acontact)
		returned-acontact)
	    nil)))))

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
	  ;; collisions with walls
					;TODO: only generate one contact if the two collisions are from the same point
	  (do-walls (pt1 pt2) obstacles
	    (let* ((wallsym (gensym))
		   (wall-state (make-state :symbol wallsym
					   :vel (make-pt-gur)
					   :pos (make-pt-board))))
	      (attribute-set wallsym
			     :shape (make-segment :pt1 pt1
						  :pt2 pt2))
	      (let ((acontact (collision-generic state wall-state)))
		(when acontact
		  (with-slots ((depth r) (normal theta)) acontact
		    (push (make-contact :depth depth
					:normal normal
					:thing thing
					:thing-pos pos
					:thing-vel vel
					:hit :wall
					:hit-vel (make-pt-gur))
			  contact-lst))))))
	  ;; collisions with other guys, do each pair once
	  (dotimes (o-thing-ndx thing-ndx)
	    (let* ((other-state (elt state0-lst o-thing-ndx)))
	      (with-slots ((o-thing symbol) (o-pos pos) (o-vel vel)) other-state
		(let* ((melee-acontact (collision-generic state other-state)))
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
	  )))
    contact-lst))

(defun collision-resolve (state0-lst obstacles)
  "returns the forces from collisions"
  (let ((contact-lst (generate-contacts state0-lst obstacles))
	(forces-plist))
    (dolist (contact contact-lst)
      (with-slots (depth normal thing thing-vel hit hit-vel) contact
	;; calculate a force for each contact
	(let* ((vel-diff (v- thing-vel
			     (or hit-vel (make-pt-gur))))
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
