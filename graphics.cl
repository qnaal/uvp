(defun unproject (x)
  (/ x *zoom*))

(defun project (x)
  (round (* x *zoom*)))

(defun project-pt (pt)
  (let ((x (pt-x pt))
	(y (pt-y pt)))
    (sdl:point :x (project x)
	       :y (project y))))

(defun draw-guy (guy)
  (let* ((pos (attribute guy :pos))
	 (pt (project-pt pos))
	 (safe (project-pt (attribute guy :safe)))
	 (r (project (attribute guy :size))))
    (sdl-gfx:draw-circle safe r :color sdl:*yellow* :aa (getf *options* :aa))
    (sdl-gfx:draw-circle pt r :color sdl:*white* :aa (getf *options* :aa))))

(defun draw-poly (poly color)
  ;;(sdl-gfx:draw-shape poly :color color :aa (getf *options* :aa))
  ;;NOTE: temporary workaround because draw-shape inexplicably doesn't
  ;;do antialiasing
  (do ((line-index 0 (+ 1 line-index))) ((= line-index (1- (length poly))))
    (let ((p1 (nth     line-index  poly))
	  (p2 (nth (1+ line-index) poly)))
      (sdl-gfx:draw-line p1 p2 :color color :aa (getf *options* :aa)))))

(defun draw-map (map color)
  (dotimes (poly-index (length map))
    (let ((poly (nth poly-index map))
	  (sdl-poly))
      (dotimes (pt-index (length poly))
	(push (project-pt (nth pt-index poly)) sdl-poly))
      (draw-poly sdl-poly color))))

(defun draw-particle (part)
  (let* ((pos (particle-pos part))
	 (theta (particle-theta part))
	 (size 10)
	 (pt1 (project-pt pos))
	 (pt0 (project-pt (v- pos
			      (carterize (make-pt-pol size theta))))))
    (sdl-gfx:draw-line pt1 pt0 :color sdl:*white* :aa (getf *options* :aa))))
