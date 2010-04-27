(let ((scalefactor 10000))
  (defun make-pt-gur (&optional (x 0) (y 0))
    (let ((x-b (* x scalefactor))
	  (y-b (* y scalefactor)))
      (make-pt-board x-b y-b)))
  (defun pt-gur-x (pt-board)
    (/ (pt-x pt-board) scalefactor))
  (defun pt-gur-y (pt-board)
    (/ (pt-y pt-board) scalefactor))
  (defun make-pt-pol-gur (&optional (r 0) (theta 0))
    (make-pt-pol-board (* scalefactor r) theta))
  (defun pt-pol-gur-r (pt-pol-board)
    (/ (pt-pol-r pt-pol-board) scalefactor))
  (defun pt-pol-gur-theta (pt-pol-board)
    (pt-pol-theta pt-pol-board))
  (defun gur-pt-to-board (pt-gur)
    (v* scalefactor pt-gur))
  (defun board-pt-to-gur (pt-board)
    (v* (/ scalefactor) pt-board))
  (defun gur-to-board (scalar)
    (* scalar scalefactor))
  (defun board-to-gur (scalar)
    (/ scalar scalefactor))
  )

(defun unproject-screen (x)
  (/ x *zoom*))

(defun project-screen (x)
  (round (* x *zoom*)))

(let ((zoom)
      (screen-size)
      (screen-middle)
      (target-on-board))

  (defun update-screen ()
    (setf
     zoom *zoom*
     screen-size *screen-size*
     screen-middle (v* 1/2 screen-size)
     target-on-board (attribute *guy* :pos)))

  (defun board-pt-to-screen (pt-board)
    (let ((pt-on-screen (v+ screen-middle (v* zoom (board-pt-to-gur (v- pt-board target-on-board))))))
      (let ((x-screen (pt-x pt-on-screen))
	    (y-screen (pt-y pt-on-screen)))
	(make-pt-screen (round x-screen) (round y-screen)))))

  (defun screen-pt-to-board (pt-screen)
    (let ((pt-on-board (v+ target-on-board (gur-pt-to-board (v* (/ zoom) (v- pt-screen screen-middle))))))
      pt-on-board)))

(defun screen-pt-to-sdl (pt-screen)
  (with-slots (x y) pt-screen
    (sdl:point :x x :y y)))
