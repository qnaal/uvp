(proclaim '(inline v- v+ make-pt pt-x pt-y))

;; (defun v+ (&rest vectors)
;;   "adds cartesian vectors"
;;   (apply 'mapcar '+ vectors))

;; (defun v+ (v1 v2)
;;   "subtracts cartesian vectors"
;;   (destructuring-bind ((x1 y1) (x2 y2))
;;       (list v1 v2)
;;     (list (+ x1 x2) (+ y1 y2))))

(defstruct vec
  x y)

;; (defstruct vec
;;   x y)

(defun make-pt (&optional (x 0) (y 0))
  (make-vec :x x :y y))
(defun pt-x (pt)
  (vec-x pt))
(defun pt-y (pt)
  (vec-y pt))

;; (defun make-pt (&optional (x 0) (y 0))
;;   (vector x y))
;; (defun pt-x (pt)
;;   (elt pt 0))
;; (defun pt-y (pt)
;;   (elt pt 1))

(defstruct vec-pol
  r theta)

(defun make-pt-pol (&optional (r 0) (theta 0))
  (make-vec-pol :r r :theta theta))
(defun pt-pol-r (pt)
  (vec-pol-r pt))
(defun pt-pol-theta (pt)
  (vec-pol-theta pt))

;; (defun make-pt-pol (&optional (r 0) (theta 0))
;;   (vector r theta))
;; (defun pt-pol-r (pt-pol)
;;   (elt pt-pol 0))
;; (defun pt-pol-theta (pt-pol)
;;   (elt pt-pol 1))

(defun v+ (&rest vectors)
  "subtracts cartesian vectors"
  (let ((output (make-pt)))
    (dotimes (i (length vectors) output)
      (let ((x (pt-x (nth i vectors)))
	    (y (pt-y (nth i vectors)))
	    (xo (pt-x output))
	    (yo (pt-y output)))
	(setf output (make-pt (+ x xo) (+ y yo)))))))

(defun v- (pt1 pt2)
  (let ((x1 (pt-x pt1))
	(y1 (pt-y pt1))
	(x2 (pt-x pt2))
	(y2 (pt-y pt2)))
    (make-pt (- x1 x2) (- y1 y2))))

(defun v* (s v)
  "scalar-multiplys a cartesian vector"
  (make-pt (* s (pt-x v))
	   (* s (pt-y v))))

(defun pythag (v)
  "returns magnitude of cartesian vector"
  (let ((x (pt-x v))
	(y (pt-y v)))
    (sqrt (+ (expt x 2)
	     (expt y 2)))))

(defun azimuth (v)
  "returns angle of cartesian vector"
  (atan (pt-y v) (pt-x v)))

(defun polarize (pt)
  "converts a cartesian vector to polar"
  (make-pt-pol (pythag pt) (azimuth pt)))

(defun carterize (pt-pol)
  "converts a polar vector to cartesian"
  (let ((r (pt-pol-r pt-pol))
	(theta (pt-pol-theta pt-pol)))
    (make-pt (* r (cos theta))
	     (* r (sin theta)))))

;; (defun distance (pt1 pt2)
;;   (pythag (v- pt2 pt1)))

;;(defun rotate (pt-pol theta)
;;  (destructuring-bind (r pre-theta) pt-pol
;;    (list r (+ theta pre-theta))))

;;(defun rotate-points (theta &rest points)
;;  (let ((rotated))
;;    (dolist (pt points (nreverse rotated))
;;      (push (carterize (rotate (polarize pt)
;;			       theta))
;;	    rotated))))

(defun component (pt theta)
  (dot pt (make-pt (cos theta)
		   (sin theta))))

(defun dot (v1 v2)
  "returns the product of the lengths of the two vectors and the cosine of the angle between them"
  (let ((x1 (pt-x v1))
	(y1 (pt-y v1))
	(x2 (pt-x v2))
	(y2 (pt-y v2)))
    (+ (* x1 x2)
       (* y1 y2))))

(defun cross (v1 v2)
  "the area of the parallelogram formed between the two vectors"
  (let ((x1 (pt-x v1))
	(y1 (pt-y v1))
	(x2 (pt-x v2))
	(y2 (pt-y v2)))
    (- (* x1 y2)
       (* x2 y1))))

(defun proj (v1 v2)
  "closest point on vec O->v2 to v1, in terms of the length of O->v2"
  (/ (dot v1 v2) (dot v2 v2)))

(defun clamp (x min max)
  (cond ((> x max) max)
	((< x min) min)
	(t x)))