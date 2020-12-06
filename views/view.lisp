
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
(defparameter *paths* nil)

(defmethod glut:display-window ((window window))
  (gl:clear :color-buffer-bit)               ; clear buffer

  (gl:ortho (* -2 *aspect-ratio*) (* 2 *aspect-ratio*) -2 2 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (if *path*
      (progn
        (gl:begin :line-loop)
        (mapc #'(lambda (c)
                  (%gl:vertex-2d (- (/ (* *mm-to-px* (car c)) *height*) 0.9)
                                 (- (/ (* *mm-to-px* (cdr c)) *width*) 0.9)))
              *path*)
        (gl:end))
      (mapc #'(lambda (path)
                (progn
                  (gl:begin :line-loop)
                  (mapc #'(lambda (c)
                            (%gl:vertex-2d
                             (- (/ (* *mm-to-px* (car c)) *height*) 0.9)
                             (- (/ (* *mm-to-px* (cdr c)) *width*) 0.9)))
                        path)
                  (gl:end)))
            *paths*))
  (glut:swap-buffers))  ; show window

;; Main
(defun view (path &optional width height)
  (setf *path* path)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))  ; create window

(defun multi-view (paths &optional width height)
  (setf *path* nil)
  (setf *paths* paths)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))

