;;;; plot-it-now.lisp
(in-package :pin)

(defun %data-range (data)
  (list (apply #'min data) (apply #'max data)))

(defgeneric plots (x-data y-datas &key lines point-size))

(defmethod plots ((x (eql nil)) y-datas &key lines point-size )
  (plots (iota (apply #'max (mapcar #'length y-datas))) y-datas))

(defun plot-lines (tr-x-points tr-y-point-lists colors)
  "Plot a line graph from X to Y, for each y-series in TR-Y-POINT-LISTS"
  (mapc
   #'(lambda (y-data color)
       (mapc #'(lambda (x1 x2 y1 y2)
		 (when (and y1 y2)
		   (apply #'vecto:set-rgb-stroke color)
		   (vecto:move-to x1 y1)
		   (vecto:line-to x2 y2)
		   (vecto:stroke)))
	     tr-x-points (cdr tr-x-points) y-data (cdr y-data)))
   tr-y-point-lists colors))

(defun plot-points (tr-x-points tr-y-point-lists colors point-size)
  "Plot a scatter for each y series "
  (mapc
   #'(lambda (y-data color)
       (mapc #'(lambda (x1 y1)
		 (when y1
		   (apply #'vecto:set-rgb-stroke color)
		   (vecto:move-to x1 y1)
		   (vecto:centered-circle-path x1 y1 point-size)
		   (vecto:fill-path)))
	     tr-x-points y-data))
   tr-y-point-lists colors))

(defun make-transform-for-data (x-range y-range)
  "Compute a proper data->screen transformation, based on the actual data"
  (make-linear-transform x-range y-range))

(defvar +font-size+ 20)
(setf +font-size+ 20)

(defun plot-axes (y-range h tr)
  (vecto:set-font (vecto:get-font "/home/mason/workspace/portfolio/ProggyClean.ttf") +font-size+)
  (vecto:set-rgb-fill  0.8 0.8 0.8)
  (vecto:set-rgb-stroke 0.8 0.8 0.8)
  (let ((n 8))
    (iterate (for i from 0 to n)
	     (for data-y = (+ (first y-range)
				 (/ (* i (- (second y-range) (first y-range)))
				    n)))
	     (for window-y = (data->window-y tr data-y h))
	     (vecto:move-to 0 window-y)
	     (vecto:line-to (linear-transform-max-x tr) window-y)
	     (vecto:stroke)
	     (vecto:draw-string
	      (floor +font-size+ 2) window-y
	      (format nil "~A" data-y)))))
(defun hex-to-rgb (hexrgb)
  (let* ((r (floor hexrgb #x10000))
         (g (floor (mod hexrgb #x10000) #x100))
         (b (mod hexrgb #x100)))
    (list (/ r 255.0) (/ g 255.0) (/ b 255.0))))
(defvar +colors-20+)
(setf +colors-20+
      (mapcar #'hex-to-rgb '(#xff0000
                             #xff8c00
                             #xffd700
                             #x00bfff
                             #x7fff00
                             #xff00ff
                             #xffff00
                             #x7cfc00
                             #xff4500
                             #x00ff00
                             #xffa500
                             #x00ff7f
                             #x00fa9a
                             #x00ffff
                             #x0000ff
                             #x0000cd
                             #x9400d3
                             #x8b008b
                             #x8b0000
                             #xff0000)))

(defmethod plots (x-data y-data-lists &key (lines t) (point-size 0))
  (let* ((width 800)
	 (height 600)
	 (x-range
	  (%data-range x-data))
	 (y-range
	  (list (apply #'min (mapcar (curry #'apply #'min) y-data-lists))
		(apply #'max (mapcar (curry #'apply #'max) y-data-lists))))
	 (tr (make-transform-for-data x-range y-range))
	 (colors
	  (if (<= (length y-data-lists) 20)
              (subseq +colors-20+ 0 (length y-data-lists))
	      (mapcar #'(lambda (y)
			  (declare (ignore y))
			  (list (random 1.0) (random 1.0) (random 1.0)))
		      y-data-lists)))
	 redraw)
    
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
		     (vecto:with-canvas (:width w :height h)
		       (when lines
			 (plot-lines tr-x-points tr-y-point-lists colors))
		       (when (> point-size 0)
			 (plot-points tr-x-points tr-y-point-lists colors point-size))
		       (plot-axes y-range h tr)
		       (sdl:vecto->surface sdl:*default-display*))
		     (sdl:update-display)))))
      
      (sdl:with-init ()
	(sdl:window width height :title-caption "Line plot" :resizable t)
	(setf (sdl:frame-rate) 60)
	(setf redraw (redraw-func width height))
	(funcall redraw)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event ()
			       (sdl:update-display))
	  (:video-resize-event (:w w :h h)
			       (setf width w height h)
			       (sdl:resize-window w h)
			       (setf redraw (redraw-func w h))
			       (funcall redraw))
	  (:key-down-event ()
			   (when (sdl:key-down-p :sdl-key-escape)
			     (sdl:push-quit-event)))
	  (:idle () (sdl:update-display)))))))

