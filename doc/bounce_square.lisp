;; https://gist.github.com/lamberta/610982

;;;Animated Bouncing Square
;;;
(require :asdf)
(asdf:load-system :cl-opengl)
(asdf:load-system :cl-glut)

;;initial square position and size
(defparameter *x* 0.0)
(defparameter *y* 0.0)
(defparameter *rsize* 25)

;;step size in x and y directions
;;(number of pixels to move each time)
(defparameter *xstep* 1.0)
(defparameter *ystep* 1.0)

;;keep track of windows changing width and height
(defparameter *window-width* nil)
(defparameter *window-height* nil)

(cffi:defcallback render-scene :void ()
  "Called to draw scene."
  ;;clear the window with current clearing
  (gl:clear :color-buffer-bit)
  ;;set the current drawing color to red
  (gl:color 1.0 0.0 0.0)
  ;;draw a filled rectangle with current color
  (gl:rect *x* *y* (+ *x* *rsize*) (- *y* *rsize*))
  ;;flush drawing commands and swap
  (glut:swap-buffers))

(cffi:defcallback timer :void ((value :int))
  "Called by GLUT library when idle (window not being resized or moved)."
  (declare (ignore value))
  ;;reverse direction when you reach left or right edge
  (if (or (> *x* (- *window-width* *rsize*)) (< *x* (- *window-width*)))
    (setf *xstep* (- *xstep*)))
  ;;reverse direction when you reach top or bottom edge
  (if (or (> *y* *window-height*) (< *y* (+ (- *window-height*) *rsize*)))
    (setf *ystep* (- *ystep*)))

  ;;actually move the square
  (incf *x* *xstep*)
  (incf *y* *ystep*)

  ;;check bounds. in case the window is made smaller while the rectangle
  ;;is bouncing and it finds itself outside the new clipping volume.
  (if (> *x* (+ *window-width* (- *rsize*) *xstep*))
    (setf *x* (- (- *window-width* *rsize*) 1))
    (if (< *x* (- (+ *window-width* *xstep*)))
      (setf *x* (- (+ *window-width* 1)))))

#|;;this is in the example, but doesn’t do anything
  ;;in fact, it doesn’t work correctly with it in
  (if (> *y* (+ *window-height* *ystep*))
    (setf *y* (- *window-height* 1))
    (if (< *y* (- (- *window-height* (+ *rsize* *ystep*))))
      (setf *y* (- (+ (- *window-height*) *rsize*) 1))))
|#

  ;;redraw the scene with new coordinates
  (glut:post-redisplay)
  (glut:timer-func 33 (cffi:callback timer) 1))


(cffi:defcallback change-size :void ((w :unsigned-int) (h :unsigned-int))
  "Called by GLUT library when the window has changed size."
  ;;prevent a divide by zero
  (if (eql h 0) (setf h 1))
  ;;set viewport to window dimensions
  (gl:viewport 0 0 w h)
  ;;reset coordinate system
  (gl:matrix-mode :projection)
  (gl:load-identity)

  ;;establish clipping volume, left, right, bottom, top, near, far
  (let ((aspect-ratio (/ w h)))
    (if (<= w h)
      (progn
        (setf *window-width* 100)
        (setf *window-height* (/ 100 aspect-ratio))
        (gl:ortho -100 100 (- *window-height*) *window-height* 1 -1))
      (progn
        (setf *window-width* (* 100 aspect-ratio))
        (setf *window-height* 100)
        (gl:ortho (- *window-width*) *window-width* -100 100 1 -1))))

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun setup-rc ()
  "Set up the rendering state."
  ;;set clear color to blue
  (gl:clear-color 0.0 0.0 1.0 1.0))

;;main program entry point
(glut:init "SBCL")
(glut:init-display-mode :double :rgba)
(glut:init-window-size 800 600)
(glut:create-window "Bounce")
(glut:display-func (cffi:callback render-scene))
(glut:reshape-func (cffi:callback change-size))
(glut:timer-func 33 (cffi:callback timer) 1)
(setup-rc)
(glut:main-loop)
