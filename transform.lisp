;;;; transform.lisp
(in-package :plot-it-now)

(defstruct (linear-transform (:constructor nil))
  min-x min-y
  max-x max-y)

(defun make-linear-transform (x-range y-range)
  (let* ((mx (first x-range))
	 (my (first y-range))
	 (dx (second x-range))
	 (dy (second y-range))
	 (x-off (* 0.05 (- dx mx)))
	 (y-off (* 0.05 (- dy my))))
    (let ((tr (make-instance 'linear-transform)))
      (with-slots (min-x max-x min-y max-y) tr
	(setf min-x (- mx x-off)
	      min-y (- my y-off)
	      max-x (+ dx x-off)
	      max-y (+ dy y-off)))
      tr)))

(defmethod data->window ((tr linear-transform) x y w h)
  "Transform a point from data-space to window-space"
  (list (data->window-x tr x w) (data->window-y tr y h)))

(defun data->window-x (tr x w)
  "Transform the x-coordinate from data-space to window-space"
  (with-slots (min-x max-x) tr
    (* w (/ (- x min-x) (- max-x min-x)))))

(defun data->window-y (tr y h)
  "Transform a y-coordinate from data-space to window-space"
  (with-slots (min-y max-y) tr
    (- h (* h (/ (- y min-y) (- max-y min-y))))))
