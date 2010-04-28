;; used for any cartesian vector
(defstruct (pt (:constructor
		make-pt
		(&optional (x 0) (y 0))))
  (x 0 :type real)
  (y 0 :type real))


;; used for any polar vector, including acontact (anonymous contact)
(defstruct (pt-pol
	     (:constructor
	      make-pt-pol
	      (&optional (r 0) (theta 0))))
  r theta)

(defstruct (pt-pol-board (:include pt-pol)
			 (:constructor
			  make-pt-pol-board
			  (&optional (r 0) (theta 0)))))

(defstruct (pt-board (:include
		      pt
		      ;; (x 0 :type integer)
		      ;; (y 0 :type integer)
		      )
		     (:constructor
		      make-pt-board
		      (&optional (x 0) (y 0)))))

(defun pt-board-x* (pt)
  (pt-x pt))
(defun pt-board-y* (pt)
  (pt-y pt))

;; Eventually I want to make this a type, but right now it is simpler just to use converting functions.
;; (defstruct (pt-gur (:include
;; 		      pt)
;; 		     (:constructor
;; 		      make-pt-gur
;; 		      (&optional (x 0) (y 0)))))

(defstruct (pt-pixel (:include
		       pt
		       (x 0 :type (signed-byte 16))
		       (y 0 :type (signed-byte 16)))
		      (:constructor
		       make-pt-pixel
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

(defun type-attribute (type attribute)
  "returns the default ATTRIBUTE for class/weapon/type TYPE"
  (or (getf (getf *class-list* type) attribute)
      (getf (getf *projectile-list* type) attribute)))

(flet ((check-obj (obj attribute)
	 (get obj attribute)))
  (defun attribute (obj attribute)
    "returns OBJ's ATTRIBUTE"
    (or (check-obj obj attribute)
	(type-attribute (check-obj obj :class) attribute)
	(type-attribute (check-obj obj :type) attribute)
	)))

(defun attribute-set (mortal &rest args)
  (if (= (length args) 2)
      (destructuring-bind (attribute value) args
	(setf (get mortal attribute) value))
      (dotimes (i (/ (length args) 2))
	(let* ((attribute (pop args))
	       (value (pop args)))
	  (attribute-set mortal attribute value)))))
