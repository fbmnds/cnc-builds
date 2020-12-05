;; load the cl-glut library and its dependencies (cl-opengl, cl-glu, cffi ..)
(asdf:operate 'asdf:load-op :cl-glut)

;; our window class
(defclass gl-window (glut:window) ()
  (:default-initargs :width 640 :height 480 :title "Tutorial 0"
    :mode '(:double :rgba :depth)))

(defvar *custom-identity* (make-array (* 4 4) :initial-element 0))

(defmacro mx4x4-ref (mx i j)
  `(aref ,mx ,(+ (* j 4) i)))

(defun setup-identity ()
  (setf (mx4x4-ref *custom-identity* 0 0) 1
	(mx4x4-ref *custom-identity* 1 1) 1
	(mx4x4-ref *custom-identity* 2 2) 1
	(mx4x4-ref *custom-identity* 3 3) 1))

;; resize the view
(defmethod glut:reshape ((w gl-window) width height)
  ;; ensure divide by zero never occurs
  (when (= height 0) (setf height 1))

  ;; set up the projection matrix
  (gl:matrix-mode :projection)
;;  (gl:load-identity)
  (gl:load-matrix *custom-identity*)
  (gl:frustum -1.0 1.0 -1.0 1.0 1.0 100.0)

  ;; set up the viewport with our new width and height
  (gl:matrix-mode :modelview)
  (gl:load-matrix *custom-identity*)
  (gl:viewport 0 0 width height))

;; our display initialization method
(defmethod glut:display-window :before ((w gl-window))
  (gl:clear-color 0 0 0 0)
  (gl:depth-func :less)
  (gl:shade-model :smooth)
  ;; (gl:cull-face :back)
  (gl:front-face :ccw))

;; our render method
(defmethod glut:display ((w gl-window))
  (gl:clear :color-buffer :depth-buffer)
  ;;(gl:load-identity)
  (gl:load-matrix *custom-identity*)

  ;; translate the camera -2.0 on the z axis
  (gl:translate 0.0 0.0 -2.0)

  ;; draw a basic triangle
  (gl:begin :triangles)
  (gl:vertex -0.5 -0.5 0.0)
  (gl:vertex 0.5 -0.5 0.0)
  (gl:vertex 0.0 0.5 0.0)
  (gl:end)

  ;; swap the gl buffers
  (glut:swap-buffers))

;; keyboard handler
(defmethod glut:keyboard ((w gl-window) key x y)
  (case key
    (#\Escape (glut:destroy-current-window))))

;; application entry point
(defun start ()
  (glut:display-window (make-instance 'gl-window)))
