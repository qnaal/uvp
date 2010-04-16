(defun unproject (x)
  (/ x *zoom*))

(defun project (x)
  (round (* x *zoom*)))

(defun project-pt (pt)
  (let ((x (pt-x pt))
	(y (pt-y pt)))
    (sdl:point :x (project x)
	       :y (project y))))

(defun draw-shape (shape pos &optional (color sdl:*default-color*))
  (let ((aa (getf *options* :aa)))
    (case (type-of shape)
      (circle (let ((r-proj (project (circle-r shape)))
		    (pos-proj (project-pt pos)))
		(sdl-gfx:draw-circle pos-proj r-proj :color color :aa aa))))))

(defun draw-guy (guy)
  (let ((shape (attribute guy :shape))
	(pos (attribute guy :pos))
	(safe (attribute guy :safe)))
    (draw-shape shape safe sdl:*red*)
    (draw-shape shape pos sdl:*white*)))

(defun draw-poly (poly color)		;FIXME: this needs to be consumed by draw-shape
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
  (let* ((pos (attribute part :pos))
	 (theta (attribute part :theta))
	 (length 10)
	 (pt1 (project-pt pos))
	 (pt0 (project-pt (v- pos
			      (carterize (make-pt-pol length theta))))))
    (sdl-gfx:draw-line pt1 pt0 :color sdl:*white* :aa (getf *options* :aa))))
