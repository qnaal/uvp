(proclaim '(inline v- v+ make-pt pt-x pt-y))

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

(defun dot (v1 v2)
  "returns the product of the lengths of the two vectors and the cosine of the angle between them"
  (let ((x1 (pt-x v1))
	(y1 (pt-y v1))
	(x2 (pt-x v2))
	(y2 (pt-y v2)))
    (+ (* x1 x2)
       (* y1 y2))))

(defun component (pt theta)
  (dot pt (make-pt (cos theta)
		   (sin theta))))

(defun cross (v1 v2)
  "the area of the parallelogram formed between the two vectors"
  (let ((x1 (pt-x v1))
	(y1 (pt-y v1))
	(x2 (pt-x v2))
	(y2 (pt-y v2)))
    (- (* x1 y2)
       (* x2 y1))))

(defun proj (pt line-pt2 &optional (line-pt1 (make-pt)))
  "closest point on line-(pt1->pt2) to pt, in terms of the length of that line"
  (let ((line (v- line-pt2 line-pt1))
	(pt-rel (v- pt line-pt1)))
    (/ (dot pt-rel line) (dot line line))))

(defun clamp (x &optional (max 1) (min 0))
  (cond ((> x max) max)
	((< x min) min)
	(t x)))

(defun invert-pol (pt-pol)
  (with-slots (r theta) pt-pol
      (make-pt-pol r (+ theta pi))))
