
(defun v+ (&rest vectors)
  "adds cartesian vectors"
  (apply #'mapcar #'+ vectors))

(defun v- (&rest vectors)
  "subtracts cartesian vectors"
  (apply #'mapcar #'- vectors))

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

(defun component (pt-pol theta)
  (car (carterize (rotate pt-pol (- theta)))))
