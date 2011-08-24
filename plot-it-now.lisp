;;;; plot-it-now.lisp
(in-package :pin)

(defgeneric scatter (x y))

(defmethod scatter ((x ir-iterator) (y function))
  (iterate (for i in-ir-iterator x)
	   (collect (list i (funcall y i)))))

(defun plot-data (data)
  (let ((width 320)
	(height 240))
    (labels ((redraw ()
	       (mapc
		#'(lambda (p)
		    (sdl-gfx:draw-filled-circle-* (first p) (second p) 10
						  :color sdl:*white*))
		data)
	       (sdl:update-display)))
      (sdl:with-init ()
	(sdl:window width height :title-caption "plot" :resizable t)
	(setf (sdl:frame-rate) 60)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-resize-event (:w w :h h)
			       (redraw))
	  (:idle ()
		 (redraw)))))))