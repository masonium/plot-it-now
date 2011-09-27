;;;; iterable-range.lisp
;;;; An iterable-range is something that can be used as a set of x values 
;;;; to plot against. It can be backed against a list, or another more abstract type.
(in-package :pin)


(defgeneric get-iterator (ir)
  (:documentation "Creates an iterator that can be used to iterate
  non-destructively over the range."))

(defgeneric copy-iterator (ir)
  (:documentation "Copies an iterator, so that it can be used later"))

(defclass ir-iterator ()
  ())

(defgeneric ir-has-next (ir)
  (:documentation "Returns true if there's another element in the range"))

(defgeneric ir-next (it)
  (:documentation "Returns the next element and advances to the next element, 
or nil if there is no next element"))

(defmacro-driver (for var in-ir-iterator iter)
  (let ((iterator (gensym))
	(kwd (if generate 'generate 'for)))
    `(progn
       (with ,iterator = ,iter)
       (,kwd ,var next (if (ir-has-next ,iterator)
			   (ir-next ,iterator)
			   (terminate))))))
 
(defgeneric ir-collect (it)
  (:documentation "Returns the remaining elements of ir")
  (:method ((it ir-iterator))
    (iterate (for var in-ir-iterator it)
	     (collect var)))
  (:method (ir)
    (iterate (for var in-ir-iterator (get-iterator ir))
	     (collect var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; list-based implemntation

(defclass list-iterator (ir-iterator)
  ((curr-cons :initarg :curr-cons)))

(defmethod get-iterator ((ir list))
  (make-instance 'list-iterator :curr-cons ir))

(defmethod ir-has-next ((it list-iterator))
  (slot-value  it 'curr-cons))

(defmethod ir-next ((it list-iterator))
  (with-slots (curr-cons) it
    (prog1 (car curr-cons)
      (setf curr-cons (cdr curr-cons)))))

;;; We can do the obvious specializations for lists here
(defmethod ir-collect ((it list-iterator))
  (copy-list (slot-value it 'curr-cons)))

(defmethod ir-collect ((it list))
  (copy-list it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; array-based

(defclass array-iterator (ir-iterator)
  ((index :initarg :index
	  :initform 0)
   (max :initarg :max)
   (arr :initarg :arr)))

(defmethod print-object ((ir array-iterator) stream)
  (print-unreadable-object (ir stream :type t :identity nil)
    (princ (slot-value ir 'index) stream)))

(defmethod get-iterator ((ir array))
  (make-instance 'array-iterator :arr ir :max (array-dimension ir 0)))

(defmethod ir-has-next ((it array-iterator))
  (with-slots (index max) it
    (< index max)))

(defmethod ir-next ((it array-iterator))
  (with-slots (index arr) it
    (prog1
	(aref arr index)
      (incf index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; linear range

(defstruct (linear-range)
  min
  max
  step)

(defun range (min max delta)
  (make-linear-range :min min :max max :step delta))

(defun step-range (min step n)
  (make-linear-range :min min :step step :max (+ min (* step (1- n)))))

(defclass lr-iterator (ir-iterator)
  ((index :initform 0 :type 'fixnum)
   (curr :initarg :curr)
   (threshold :initarg :threshold)
   (range :initarg :range)))

(defmethod print-object ((range linear-range) stream)
  (print-unreadable-object (range stream :type t :identity nil)
    (with-slots (min max step) range
      (format stream "~A ~A ~A" min max step))))

(defmethod get-iterator ((lr linear-range))
  (make-instance 'lr-iterator :range lr :curr (linear-range-min lr)
		 :threshold (+ (linear-range-max lr) (/ (linear-range-step lr) 100))))

(defmethod ir-has-next ((lri lr-iterator))
  (with-slots (curr threshold) lri
    (< curr threshold)))

(defmethod ir-next ((lri lr-iterator))
  (with-slots (index curr range) lri
    (prog1
      curr
      (setf curr (+ (linear-range-min range) (* index (linear-range-step range))))
      (incf index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Reference Type
(deftype iterable-range ()
  `(or (and (satisfies listp)
	    (not (satisfies null)))
       (satisfies arrayp)
       linear-range))
