(defun draw-line (pt1-pixel pt2-pixel &optional (color sdl:*default-color*))
  (let ((aa (getf *options* :aa))
	(pt1-sdl (screen-pt-to-sdl pt1-pixel))
	(pt2-sdl (screen-pt-to-sdl pt2-pixel)))
    (sdl-gfx:draw-line pt1-sdl pt2-sdl :color color :aa aa)))

(defun draw-circle (pos-sc r-sc &optional (color sdl:*default-color*))
  (let ((aa (getf *options* :aa))
	(pos-sdl (screen-pt-to-sdl pos-sc)))
    (sdl-gfx:draw-circle pos-sdl r-sc :color color :aa aa)))

(defun draw-shape (shape pos &optional (color sdl:*default-color*))
  (case (type-of shape)
    (circle (let ((r-pixel (project-screen (circle-r shape)))
		  (pos-screen (board-pt-to-screen pos)))
	      (draw-circle pos-screen r-pixel color)))))

(defun draw-guy (guy)
  (let ((shape (attribute guy :shape))
	(pos (attribute guy :pos))
	(safe (attribute guy :safe)))
    (draw-shape shape safe sdl:*red*)
    (draw-shape shape pos sdl:*white*)))

(defun draw-particle (part)
  (let* ((pos (attribute part :pos))
	 (theta (attribute part :theta))
	 (length 10)
	 (pt1 (board-pt-to-screen pos))
	 (pt0 (board-pt-to-screen (v- pos
			      (carterize (make-pt-pol-gur length theta))))))
    (draw-line pt1 pt0 sdl:*white*)))

(defun draw-map (map color)
  (do-walls (pt1 pt2) map
    (let ((pt1-pixel (board-pt-to-screen pt1))
	  (pt2-pixel (board-pt-to-screen pt2)))
      (draw-line pt1-pixel pt2-pixel color))))
