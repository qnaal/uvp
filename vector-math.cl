;; (defun v+ (&rest vectors)
;;   "adds cartesian vectors"
;;   (apply 'mapcar '+ vectors))

;; (defun v+ (v1 v2)
;;   "subtracts cartesian vectors"
;;   (destructuring-bind ((x1 y1) (x2 y2))
;;       (list v1 v2)
;;     (list (+ x1 x2) (+ y1 y2))))

(defun v+ (&rest vectors)
  "subtracts cartesian vectors"
  (if (= 2 (length vectors))
      (destructuring-bind ((x1 y1) (x2 y2))
	  (list (first vectors)
		(second vectors))
	(list (+ x1 x2) (+ y1 y2)))
      (apply #'mapcar #'+ vectors)))

;; (defun v- (&rest vectors)
;;   "subtracts cartesian vectors"
;;   (apply 'mapcar '- vectors))

;; (declaim (inline v-))
(defun v- (v1 v2)
  "subtracts cartesian vectors"
  (destructuring-bind (x1 y1) v1
    (destructuring-bind (x2 y2) v2
      (list (- x1 x2) (- y1 y2)))))

(defun v* (s v)
  "scalar-multiplys a cartesian vector"
  (let ((output))
    (dolist (component v (nreverse output))
      (push (* s component) output))))

(defun pythag (v)
  "returns magnitude of cartesian vector"
  (destructuring-bind (x y) v
    (sqrt (+ (expt x 2)
	     (expt y 2)))))

(defun azimuth (v)
  "returns angle of cartesian vector"
  (destructuring-bind (x y) v
    (atan y x)))

(defun polarize (pt)
  "converts a cartesian vector to polar"
  (destructuring-bind (x y) pt
    (list (pythag pt) (atan y x))))

(defun carterize (pt)
  "converts a polar vector to cartesian"
  (destructuring-bind (r theta) pt
    (list (* r (cos theta))
	  (* r (sin theta)))))

;; (defun distance (pt1 pt2)
;;   (pythag (v- pt2 pt1)))

(defun rotate (pt-pol theta)
  (destructuring-bind (r pre-theta) pt-pol
    (list r (+ theta pre-theta))))

(defun rotate-points (theta &rest points)
  (let ((rotated))
    (dolist (pt points (nreverse rotated))
      (push (carterize (rotate (polarize pt)
			       theta))
	    rotated))))

(defun component (pt theta)
  (dot pt (list (cos theta)
		(sin theta))))

;; (defun dot (v1 v2)
;;   (apply '+ (apply 'mapcar '* (list v1 v2))))

(defun dot (v1 v2)
  "returns the product of the lengths of the two vectors and the cosine of the angle between them"
  (destructuring-bind (x1 y1) v1
    (destructuring-bind (x2 y2) v2
      (+ (* x1 x2)
	 (* y1 y2)))))

(defun proj (v1 v2)
  "closest point on vec O->v2 to v1, in terms of the length of O->v2"
  (/ (dot v1 v2) (dot v2 v2)))

(defun clamp (x min max)
  (cond ((> x max) max)
	((< x min) min)
	(t x)))

(defun pt-x (pt)
  (first pt))

(defun pt-y (pt)
  (second pt))

(defun make-pt (x y)
  (list x y))

(flet ((v- (pt1 pt2)
	 (let ((x1 (pt-x pt1))
	       (y1 (pt-y pt1))
	       (x2 (pt-x pt2))
	       (y2 (pt-y pt2)))
	   (make-pt (- x1 x2) (- y1 y2))))
       (pythag (pt)
	 (let ((x (pt-x pt))
	       (y (pt-y pt)))
	   (+ (expt x 2) (expt y 2))))
       (azimuth (pt)
	 (let ((x (pt-x pt))
	       (y (pt-y pt)))
	   (atan y x))))
  (defun collision-circle-circle-abst (circle1-pt circle2-pt min-dist)
    ;;  (declare (inline v- pythag azimuth))
    (let* ((v-diff (v- circle2-pt circle1-pt))
	   (dist (pythag v-diff)))
      (when (< dist min-dist)
	(make-pt (- min-dist dist) (azimuth v-diff))))))

(labels ((pt-x (pt)
	   (elt pt 0))
	 (pt-y (pt)
	   (elt pt 1))
	 (make-pt (x y)
	   (vector x y))
	 (v- (pt1 pt2)
	   (let ((x1 (pt-x pt1))
		 (y1 (pt-y pt1))
		 (x2 (pt-x pt2))
		 (y2 (pt-y pt2)))
	     (make-pt (- x1 x2) (- y1 y2))))
	 (pythag (pt)
	   (let ((x (pt-x pt))
		 (y (pt-y pt)))
	     (+ (expt x 2) (expt y 2))))
	 (azimuth (pt)
	   (let ((x (pt-x pt))
		 (y (pt-y pt)))
	     (atan y x))))
  (declare (inline pt-x pt-y make-pt v- pythag azimuth))
  (defun collision-circle-circle-veca (circle1-pt circle2-pt min-dist)
    (let* ((v-diff (v- circle2-pt circle1-pt))
	   (dist (pythag v-diff)))
      (when (< dist min-dist)
	(make-pt (- min-dist dist) (azimuth v-diff))))))

(flet ((v- (pt1 pt2)
	 (let ((x1 (elt pt1 0))
	       (y1 (elt pt1 1))
	       (x2 (elt pt2 0))
	       (y2 (elt pt2 1)))
	     (vector (- x1 x2) (- y1 y2))))
       (pythag (pt)
	 (let ((x (elt pt 0))
	       (y (elt pt 1)))
	   (+ (expt x 2) (expt y 2))))
       (azimuth (pt)
	 (let ((x (elt pt 0))
	       (y (elt pt 1)))
	   (atan y x))))
  (declare (inline v- pythag azimuth))
  (defun collision-circle-circle-vec (circle1-pt circle2-pt min-dist)
    ;;  (declare (inline v- pythag azimuth))
    (let* ((v-diff (v- circle2-pt circle1-pt))
	   (dist (pythag v-diff)))
      (when (< dist min-dist)
	(vector (- min-dist dist) (azimuth v-diff))))))

(defstruct stv
  x y)

(flet ((v- (pt1 pt2)
	 (let ((x1 (stv-x pt1))
	       (y1 (stv-y pt1))
	       (x2 (stv-x pt2))
	       (y2 (stv-y pt2)))
	     (make-stv :x (- x1 x2) :y (- y1 y2))))
       (pythag (pt)
	 (let ((x (stv-x pt))
	       (y (stv-y pt)))
	   (+ (expt x 2) (expt y 2))))
       (azimuth (pt)
	 (let ((x (stv-x pt))
	       (y (stv-y pt)))
	   (atan y x))))
  (defun collision-circle-circle-stv (circle1-pt circle2-pt min-dist)
    ;;  (declare (inline v- pythag azimuth))
    (let* ((v-diff (v- circle2-pt circle1-pt))
	   (dist (pythag v-diff)))
      (when (< dist min-dist)
	(make-stv :x (- min-dist dist) :y (azimuth v-diff))))))

(defstruct (stvv (:type vector))
  x y)

(flet ((v- (pt1 pt2)
	 (let ((x1 (stvv-x pt1))
	       (y1 (stvv-y pt1))
	       (x2 (stvv-x pt2))
	       (y2 (stvv-y pt2)))
	     (make-stvv :x (- x1 x2) :y (- y1 y2))))
       (pythag (pt)
	 (let ((x (stvv-x pt))
	       (y (stvv-y pt)))
	   (+ (expt x 2) (expt y 2))))
       (azimuth (pt)
	 (let ((x (stvv-x pt))
	       (y (stvv-y pt)))
	   (atan y x))))
  (defun collision-circle-circle-stvv (circle1-pt circle2-pt min-dist)
    ;;  (declare (inline v- pythag azimuth))
    (let* ((v-diff (v- circle2-pt circle1-pt))
	   (dist (pythag v-diff)))
      (when (< dist min-dist)
	(make-stvv :x (- min-dist dist) :y (azimuth v-diff))))))
