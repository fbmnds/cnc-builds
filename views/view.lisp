
(in-package #:paths/view)

(defparameter *mm-to-px* 3.7795275591)
(defparameter *width* 100)
(defparameter *height* 100)

(defclass window (glut:window) 
  ()
  (:default-initargs
   :width *width* :height *height*
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
  (mapc #'(lambda (c)
            (%gl:vertex-2d (/ (* *mm-to-px* (car c)) *height*)
                           (/ (* *mm-to-px* (cdr c)) *height*)))
        *path*)
  (gl:end)
  (gl:flush))                                ; show window

;; Main
(defun view (path &optional width height)
  (setf *path* path)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (glut:display-window (make-instance 'window)))  ; create window


