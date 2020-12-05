
(in-package #:paths/view)

(defparameter *mm-to-px* 3.7795275591)
(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *aspect-ratio* 1.0)

(defclass window (glut:window) 
  ()
  (:default-initargs
   :width *width* :height *height*
   :mode '(:rgba)))

;; Init
(defmethod glut:display-window :before ((window window))
  (gl:clear-color 0 0 1 1))                  ; set blue

(defmethod glut:display-window :after ((window window))
  (glut:position-window 100 500))

;; Display
(defparameter *path* nil)

(defmethod glut:display ((window window))
  (gl:clear :color-buffer-bit)               ; clear buffer

  (gl:ortho (* -2 *aspect-ratio*) (* 2 *aspect-ratio*) -2 2 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (gl:begin :line-loop)
  (mapc #'(lambda (c)
            (%gl:vertex-2d (/ (* *mm-to-px* (car c)) *height*)
                           (/ (* *mm-to-px* (cdr c)) *width*)))
        *path*)
  (gl:end)
  (glut:swap-buffers))                                ; show window

;; Main
(defun view (path &optional width height)
  (setf *path* path)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))  ; create window

(defparameter l1 '((1 . 1) (11 . 1) (11 . 5) (1 . 5)))
(defparameter l2 '((0 . 0) (10 . 0) (10 . 5) (15 . 5)))

(defparameter d1-ac 4)
(defparameter d2-ac 4)
(defparameter d1-bd 4)
(defparameter d2-bd 4)
(defparameter dy  2.5)
(defparameter eps 0.01)

(defparameter lx (+ 50 (* 2 dy)))
(defparameter ly (+ 20 (* 2 dy)))
(defparameter lz ly)
(defparameter spacer-x (+ lx 4))
(defparameter spacer-y (+ ly 4))
(defparameter spacer-z (+ lz 4))

(defparameter tbox
  (paths/box:box lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-x spacer-y spacer-z))


