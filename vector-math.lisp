;; (proclaim '(inline v- v+ make-pt pt-x pt-y))

;; (defun v+ (&rest vectors)
;;   "adds cartesian vectors"
;;   (apply 'mapcar '+ vectors))

;; (defun v+ (v1 v2)
;;   "subtracts cartesian vectors"
;;   (destructuring-bind ((x1 y1) (x2 y2))
;;       (list v1 v2)
;;     (list (+ x1 x2) (+ y1 y2))))

(defun v+ (&rest pts)
  "adds cartesian vectors"
  (let ((output (make-pt)))
    (dotimes (i (length pts) output)
      (let ((x (pt-x (nth i pts)))
	    (y (pt-y (nth i pts)))
	    (xo (pt-x output))
	    (yo (pt-y output)))
	(setf output (make-pt (+ x xo) (+ y yo)))))))

(defun v- (pt1 pt2)	  ;v- was profiled to be consing 4x what v+ is
  "subtracts two cartesian vectors"
  (let ((x1 (pt-x pt1))
	(y1 (pt-y pt1))
	(x2 (pt-x pt2))
	(y2 (pt-y pt2)))
    (make-pt (- x1 x2) (- y1 y2))))

(defun v* (s pt)
  "scalar-multiplys a cartesian vector"
  (make-pt (* s (pt-x pt))
	   (* s (pt-y pt))))

(defun v= (pt1 pt2)
  "compares two cartesian vectors"
  (with-slots ((x1 x) (y1 y)) pt1
    (with-slots ((x2 x) (y2 y)) pt2
      (and (= x1 x2)
	   (= y1 y2)))))

(defun pythag (pt)
  "returns magnitude of cartesian vector"
  (let ((x (pt-x pt))
	(y (pt-y pt)))
    (sqrt (+ (expt x 2)
	     (expt y 2)))))

(defun azimuth (pt)
  "returns angle of cartesian vector"
  (atan (pt-y pt) (pt-x pt)))

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

(defun dot (pt1 pt2)
  "returns the product of the lengths of the two vectors and the cosine of the angle between them"
  (let ((x1 (pt-x pt1))
	(y1 (pt-y pt1))
	(x2 (pt-x pt2))
	(y2 (pt-y pt2)))
    (+ (* x1 x2)
       (* y1 y2))))

(defun component (pt theta)
  (dot pt (make-pt (cos theta)
		   (sin theta))))

(defun cross (pt1 pt2)
  "the area of the parallelogram formed between the two vectors"
  (let ((x1 (pt-x pt1))
	(y1 (pt-y pt1))
	(x2 (pt-x pt2))
	(y2 (pt-y pt2)))
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
