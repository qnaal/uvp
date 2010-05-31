(let ((board-granularity 10000))
  (defun make-pt-gur (&optional (x 0) (y 0))
    (let ((x-b (* x board-granularity))
	  (y-b (* y board-granularity)))
      (make-pt-board x-b y-b)))
  (defun pt-gur-x (pt-board)
    (/ (pt-x pt-board) board-granularity))
  (defun pt-gur-y (pt-board)
    (/ (pt-y pt-board) board-granularity))
  (defun make-pt-pol-gur (&optional (r 0) (theta 0))
    (make-pt-pol-board (* board-granularity r) theta))
  (defun pt-pol-gur-r (pt-pol-board)
    (/ (pt-pol-r pt-pol-board) board-granularity))
  (defun pt-pol-gur-theta (pt-pol-board)
    (pt-pol-theta pt-pol-board))
  (defun gur-pt-to-board (pt-gur)
    (v* board-granularity pt-gur))
  (defun board-pt-to-gur (pt-board)
    (v* (/ board-granularity) pt-board))
  (defun gur-to-board (scalar)
    (* scalar board-granularity))
  (defun board-to-gur (scalar)
    (/ scalar board-granularity))
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
	(make-pt-pixel (round x-screen) (round y-screen)))))

  (defun screen-pt-to-board (pt-pixel)
    (let ((pt-on-board (v+ target-on-board (gur-pt-to-board (v* (/ zoom) (v- pt-pixel screen-middle))))))
      pt-on-board)))

(defun screen-pt-to-sdl (pt-pixel)
  (with-slots (x y) pt-pixel
    (sdl:point :x x :y y)))
