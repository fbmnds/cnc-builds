
(in-package #:paths/view)

(defparameter *mm-to-px* 3.7795275591)
(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *aspect-ratio* 1.0)
(defparameter *path* nil)
(defparameter *paths* nil)
(defparameter *stats* nil)


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
  (glut:position-window 100 300))


;; Display
(defun c-vertex-2d (c)
  (gl:vertex (- (/ (* *mm-to-px* 2 (car c)) *width*) 0.9)
             (- (/ (* *mm-to-px* 2 (cdr c)) *height*) 0.9)))


(defun display-window (&optional window)
  (gl:clear :color-buffer-bit)
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

  (gl:flush)
  (glut:swap-buffers))

(defmethod glut:display ((w window))
  (display-window w))

(defmethod glut:reshape ((w window) width height)
  (declare (ignore width))
  (setf *width* (* height *aspect-ratio*))
  (setf *height* height)
  (display-window w))

(defmethod glut:keyboard ((w window) key x y)
  (declare (ignore x y))
  (when (eql key #\m) (glut:post-redisplay))
  (when (or (eql key #\Esc) (eql key #\Space))
    (glut:destroy-current-window)))

;;(setf glut:*run-main-loop-after-display* nil)

(defun run-view ()
  (setf *height* (floor (* (1+ *mm-to-px*)
                           (- (nth (1+ (position :max-y *stats*)) *stats*)
                              (nth (1+ (position :min-y *stats*)) *stats*)))))
  (setf *width* (floor (* (1+ *mm-to-px*)
                          (- (nth (1+ (position :max-x *stats*)) *stats*)
                             (nth (1+ (position :min-x *stats*)) *stats*)))))
  (setf *aspect-ratio* (/ *width* *height*))
  (glut:display-window (make-instance 'window)))

;; Main
(defun view (path &optional width height)
  (declare (ignore width height))
  (setf *path* path)
  (setf *stats* (paths:stats path))
  (run-view))

(defun multi-view (paths &optional width height)
  (declare (ignore width height))
  (setf *path* nil)
  (setf *paths* paths)
  (setf *stats* (paths:stats-acc paths))
  (run-view))

