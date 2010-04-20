;; used for any cartesian vector
(defstruct (pt (:constructor
		make-pt
		(&optional (x 0) (y 0))))
  x y)


;; used for any polar vector, including acontact (anonymous contact)
(defstruct (pt-pol
	     (:constructor
	      make-pt-pol
	      (&optional (r 0) (theta 0))))
  r theta)

(defstruct (pt-screen (:include
		       pt
		       (x 0 :type (signed-byte 16))
		       (y 0 :type (signed-byte 16)))
		      (:constructor
		       make-pt-screen
		       (&optional (x 0) (y 0)))))

;; physics state of an object- also used for dstate, where it contains
;;the derivatives of pos and vel
(defstruct state
  (symbol 0 :type symbol)
  (pos () :type pt)
  (safe ())
  (vel () :type pt))

;; normal is the _penetration normal_, ie the force will need to be in the opposite direction to correct it
;; The contact information used in calculating the results of collisions
(defstruct contact
  depth
  normal				;the direction of the collision
  (thing () :type symbol)		;the thing that hit something
  (thing-pos () :type pt)
  (thing-vel () :type pt)
  (hit () :type symbol)			;the something that thing hit
  hit-pos
  (hit-vel () :type pt))

(defmacro do-wallsegments ((pt1 pt2) poly &body body)
  (let ((wall-ndx (gensym)))
    `(dotimes (,wall-ndx (1- (length ,poly)))
       (let ((,pt1 (nth     ,wall-ndx  ,poly))
	     (,pt2 (nth (1+ ,wall-ndx) ,poly)))
	 ,@body))))

(defmacro do-walls ((pt1 pt2) map &body body)
  (let ((poly (gensym)))
    `(dolist (,poly ,map)
       (do-wallsegments (,pt1 ,pt2) ,poly
	 ,@body))))
