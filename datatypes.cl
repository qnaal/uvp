;; used for any cartesian vector
(defstruct vec
  x y)
(defun make-pt (&optional (x 0) (y 0))
  (make-vec :x x :y y))
(defun pt-x (pt)
  (vec-x pt))
(defun pt-y (pt)
  (vec-y pt))
(deftype pt () 'vec)

;; (defun make-pt (&optional (x 0) (y 0))
;;   (vector x y))
;; (defun pt-x (pt)
;;   (elt pt 0))
;; (defun pt-y (pt)
;;   (elt pt 1))

;; used for any polar vector, including acontact (anonymous contact)
(defstruct vec-pol
  r theta)
(defun make-pt-pol (&optional (r 0) (theta 0))
  (make-vec-pol :r r :theta theta))
(defun pt-pol-r (pt)
  (vec-pol-r pt))
(defun pt-pol-theta (pt)
  (vec-pol-theta pt))
(deftype pt-pol () 'vec-pol)

;; (defun make-pt-pol (&optional (r 0) (theta 0))
;;   (vector r theta))
;; (defun pt-pol-r (pt-pol)
;;   (elt pt-pol 0))
;; (defun pt-pol-theta (pt-pol)
;;   (elt pt-pol 1))

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
