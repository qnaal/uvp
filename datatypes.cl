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
