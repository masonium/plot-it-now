;;;; plot-it-now.lisp
(in-package :pin)

(defun %data-range (data)
  (list (apply #'min data) (apply #'max data)))

(defgeneric plots (x-data y-datas &key lines point-size))

(defmethod plots ((x (eql nil)) y-datas &key lines point-size )
  (plots (iota (apply #'max (mapcar #'length y-datas))) y-datas))

(defmethod plots (x-data y-data-lists &key (lines t) (point-size 0))
  (let* ((width 800)
	 (height 600)
	 (tr (make-linear-transform
	      (%data-range x-data)
	      (list (apply #'min (mapcar (curry #'apply #'min) y-data-lists))
		    (apply #'max (mapcar (curry #'apply #'max) y-data-lists)))))
	 (colors (mapcar #'(lambda (y)
			     (declare (ignore y))
			     (sdl:color :r (random 256) :g (random 256)
					:b (random 256)))
			 y-data-lists))
	 redraw )
    (print (list (length x-data) (length y-data-lists) (length (first y-data-lists))))
    (labels ((redraw-func (w h)
	       (let ((tr-x-points
		      (mapcar #'(lambda (a) (round (data->window-x tr a w))) x-data))
		     (tr-y-point-lists
		      (mapcar
		       #'(lambda (y-data)
			   (mapcar #'(lambda (b) (when b (round (data->window-y tr b h)))) y-data))
		       y-data-lists)))
		 #'(lambda ()
		     (sdl:clear-display (sdl:color))
		     (when lines
		       (mapc
			#'(lambda (y-data color)
			    (mapc #'(lambda (x1 x2 y1 y2)
				      (when (and y1 y2)
					(sdl-gfx:draw-line-* x1 y1 x2 y2 :aa t :color color :clipping nil)))
				  tr-x-points (cdr tr-x-points) y-data (cdr y-data)))
			tr-y-point-lists colors))
		     (when (> point-size 0)
		       		       (mapc
			#'(lambda (y-data color)
			    (mapc #'(lambda (x1 y1)
				      (when y1 
					(sdl-gfx:draw-circle-* x1 y1 point-size :aa t :color color)))
				  tr-x-points y-data))
			tr-y-point-lists colors))
		     (sdl:update-display)))))
      (sdl:with-init ()
	(sdl:window width height :title-caption "Line plot" :resizable t)
	(setf (sdl:frame-rate) 60)
	(setf redraw (redraw-func width height))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-resize-event (:w w :h h)
			       (setf width w height h)
			       (sdl:resize-window w h)
			       (setf redraw (redraw-func w h)))
	  (:key-down-event ()
			   (when (sdl:key-down-p :sdl-key-escape)
			     (sdl:push-quit-event)))
	  (:idle ()
		 (funcall redraw)))))))

