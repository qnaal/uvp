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