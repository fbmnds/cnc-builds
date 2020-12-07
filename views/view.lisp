
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
  (gl:clear-color 0 0 1 1)
  (gl:shade-model :flat))


(defmethod glut:display-window :after ((window window))
  (glut:position-window 100 500))


;; Display
(defparameter *path* nil)
(defparameter *paths* nil)

(defun c-vertex-2d- (c)
  (%gl:vertex-2d (- (/ (* *mm-to-px* 2 (car c)) *height*) 0.9)
                 (- (/ (* *mm-to-px* 2 (cdr c)) *width*) 0.9)))

(defun c-vertex-2d (c)
  (gl:vertex (- (/ (* *mm-to-px* 2 (car c)) *height*) 0.9)
             (- (/ (* *mm-to-px* 2 (cdr c)) *width*) 0.9)))



(defmethod glut:display-window ((window window))
  (gl:clear :color-buffer-bit)               ; clear buffer
  ;; Select white for all lines.
  (gl:color 1 1 1 1)

  (if *path*
      (progn
        (gl:begin :line-loop)
        (mapc #'c-vertex-2d *path*)
        (gl:end))
      (mapc #'(lambda (path)
                (progn
                  (gl:begin :line-loop)
                  (mapc #'c-vertex-2d path)
                  (gl:end)))
            *paths*))
  
  (glut:swap-buffers)
  (glut:post-redisplay)
  ;;(gl:flush)
  )

#|
(defmethod glut:display ((window window))
  ;;(gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer)               ; clear buffer
  ;; Select color for all lines.
  (gl:color 1 0 1)
  
  (gl:enable :line-stipple)
  (gl:line-stipple 1 #b0000000100000001) ; dotted
  
  (if *path*
      (progn
        (gl:begin :line-loop)
        (gl:color 1 0 1)
        (mapc #'c-vertex-2d *path*)
        (gl:end))
      (mapc #'(lambda (path)
                (progn
                  (gl:begin :line-loop)
                  (gl:color 1 0 1)
                  (mapc #'c-vertex-2d path)
                  (gl:end)))
            *paths*))
  ;;

  (gl:disable :line-stipple)

  (glut:swap-buffers)
  
  ;;(gl:flush)
  ;; (glut:post-redisplay)
  )  ; show window
|#

(defmethod glut:reshape ((w window) width height)
  (gl:viewport 0 0 width height)
  #|
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height)
  |#
  ;;(gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer)               ; clear buffer
  ;; Select color for all lines.
  (gl:color 1 0 1)
  
  ;;(gl:enable :line-stipple)
  ;;(gl:line-stipple 1 #b0000000100000001) ; dotted
  
  (if *path*
      (progn
        (gl:begin :line-loop)
        (gl:color 1 0 1)
        (mapc #'c-vertex-2d *path*)
        (gl:end))
      (mapc #'(lambda (path)
                (progn
                  (gl:begin :line-loop)
                  (gl:color 1 0 1)
                  (mapc #'c-vertex-2d path)
                  (gl:end)))
            *paths*))
  ;;

  ;;(gl:disable :line-stipple)

  (glut:swap-buffers)
  
  ;;(glut:post-redisplay)
  )


(defmethod glut:keyboard ((w window) key x y)
  (declare (ignore x y))
  (when (eql key #\m) (glut:post-redisplay))
  (when (or (eql key #\Esc) (eql key #\Space))
    (glut:destroy-current-window)))

;;(setf glut:*run-main-loop-after-display* nil)

;; Main
(defun view (path &optional width height)
  (setf *path* path)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))

(defun multi-view (paths &optional width height)
  (setf *path* nil)
  (setf *paths* paths)
  (when width (setf *width* (floor (* *mm-to-px* 1.1 width))))
  (when height (setf *height* (floor (* *mm-to-px* 1.1 height))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))

