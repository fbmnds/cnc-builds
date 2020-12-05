
(in-package #:paths/view)

(defclass window (glut:window) 
  ()
  (:default-initargs
   :mode '(:rgba)))

;; Init
(defmethod glut:display-window :before ((window window))
  (gl:clear-color 0 0 1 1))                  ; set blue

;; Display
(defparameter *path* nil)

(defmethod glut:display ((window window))
  (gl:clear :color-buffer-bit)               ; clear buffer
                                        ; draw a square
  (gl:begin :line-loop)
  (mapc #'(lambda (c) (%gl:vertex-2d (car c) (cdr c))) *path*)
  (gl:end)
  (gl:flush))                                ; show window

;; Main
(defun view (path)
  (setf *path* path)
  (glut:display-window (make-instance 'window) path))  ; create window


