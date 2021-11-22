
(in-package #:paths/view)

(defparameter *mm-to-px* 3.7795275591)
(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *aspect-ratio* 1.0)
(defparameter *path* nil)
(defparameter *paths* nil)
(defparameter *colored-paths* nil)
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
(defun c-vertex-2d- (c)
  (gl:vertex (- (/ (* *mm-to-px* 2 (car c)) *width*) 0.9)
             (- (/ (* *mm-to-px* 2 (cdr c)) *height*) 0.9)))

(defun c-vertex-2d (c)
  (ignore-errors
   (cond ((> *aspect-ratio* 1)
          (gl:vertex (- (/ (* *mm-to-px* 2 (car c)) *width*) 0.9)
                     (- (/ (* *mm-to-px* 2 (cdr c))
                           (/ *width* *aspect-ratio*))
                        0.9)))
         (t
          (gl:vertex (- (/ (* *mm-to-px* 2 (car c))
                           (* *height* *aspect-ratio*))
                        0.9)
                     (- (/ (* *mm-to-px* 2 (cdr c)) *height*) 0.9))))))

(defun set-color (color)
  (cond ((eq color :black) (gl:color 0 0 0 1))
        ((eq color :red) (gl:color 1 0 0 1))
        ((eq color :green) (gl:color 0 1 0 1))
        ((eq color :yellow) (gl:color 1 1 0 1))
        ((eq color :blue) (gl:color 0 0 1 1))
        ((eq color :magenta) (gl:color 1 0 1 1))
        ((eq color :cyan) (gl:color 0 1 1 1))
        ((eq color :white) (gl:color 1 1 1 1))
        (t (apply #'gl:color color))))

(defun display-window (&optional window)
  (declare (ignore window))
  (gl:clear :color-buffer-bit)

  (cond ((car *path*)
         (gl:begin :line-loop)
         (gl:color 1 1 1 1)
         (mapc #'c-vertex-2d *path*)
         (gl:end))
        ((car *paths*)
         (mapc #'(lambda (path)
                   (progn
                     (gl:begin :line-loop)
                     (gl:color 1 1 1 1)
                     (mapc #'c-vertex-2d path)
                     (gl:end)))
               *paths*))
        ((car *colored-paths*)
         (mapc #'(lambda (cpath)
                   (gl:begin :line-loop)
                   (set-color (car cpath))
                   (mapc #'c-vertex-2d (cdr cpath))
                   (gl:end))
               *colored-paths*))
        (t nil))

  (gl:flush)
  (glut:swap-buffers))

(defmethod glut:display ((w window))
  (display-window w))

(defmethod glut:reshape ((w window) width height)
  (let ((ar (/ width (max 1 height))))
    (cond ((< ar *aspect-ratio*)
           (setf *width* (round (* height ar)))
           (setf *height* height)
           (display-window w))
          ((> ar *aspect-ratio*)
           (setf *width* width)
           (setf *height* (round (/ *width* ar)))
           (display-window w))
          (t nil))))

(defmacro reset-scale ()
  `(progn
     (setf *height* (floor (* 1.1 *mm-to-px*
                              (- (getf *stats* :max-y)
                                 (getf *stats* :min-y)))))
     (setf *width* (floor (* 1.1 *mm-to-px*
                             (- (getf *stats* :max-x)
                                (getf *stats* :min-x)))))
     (setf *aspect-ratio* (/ *width* *height*))))

(defmethod glut:keyboard ((w window) key x y)
  (declare (ignore x y))
  (cond ((eql key #\0)
         (reset-scale)
         (glut:reshape-window *width* *height*)
         (glut:post-redisplay))
        ((or (eql key #\Esc) (eql key #\Space))
         (glut:destroy-current-window))
        (t nil)))

;;(setf glut:*run-main-loop-after-display* nil)

(defun run-view ()
  (reset-scale)
  (glut:display-window (make-instance 'window)))

;; Main
(defun view (path &optional width height)
  (declare (ignore width height))
  (setf *path* path)
  (setf *paths* nil)
  (setf *colored-paths* nil)
  (setf *stats* (paths:stats path))
  (run-view))

(defun multi-view (paths &optional width height)
  (declare (ignore width height))
  (setf *path* nil)
  (setf *paths* paths)
  (setf *colored-paths* nil)
  (setf *stats* (paths:stats-acc paths))
  (run-view))

(defun colored-multi-view (colored-paths &optional width height)
  (declare (ignore width height))
  (setf *path* nil)
  (setf *paths* nil)
  (setf *colored-paths* colored-paths)
  (setf *stats* (paths:stats-acc
                 (mapcar #'(lambda (cp) (cdr cp)) colored-paths)))
  (run-view))

