;; https://gist.github.com/soma-arc/525d4cd4db207c7a5e9c

;; $ echo "text!" > /dev/$PROTO/$HOST/$PORT

(ql:quickload '(:usocket :osc :cl-glut :cl-glu :bordeaux-threads))

(defparameter *port* 9000)

(defun create-server (port)
  (usocket:socket-connect nil nil
			  :protocol :datagram
			  :element-type '(unsigned-byte 8)
			  :local-host "127.0.0.1"
			  :local-port port))

(defun start-receiving (socket buffer)
  (format t "start server~%")
  (unwind-protect
       (multiple-value-bind (buffer size client received-port)
	   (loop do
                 (usocket:socket-receive socket buffer 1024)
                 (let ((message (osc:decode-bundle buffer)))
                   (if (string= (car message) "stop") (return))
                   (process-msg message))))
    (usocket:socket-close socket)
    (format t "exit~%")))

(defun process-msg (message)
  (if (string= "/audio/loud" (car message)) (setf *scale* (+ 0.2 (* 3 (second message)))))
  (if (string= "/audio/fft" (car message)) (progn
					     (setf *diffuse-r* (+ 0.3 (* 2 (second message))))
					     (setf *diffuse-g* (+ 0.3 (* 2 (elt message 5))))
					     (setf *diffuse-b* (+ 0.3 (* 2 (elt message 10)))))))

(defun start-osc-listen (port)
  (start-receiving (create-server *port*)
                   (make-array 1024 :element-type '(unsigned-byte 8))))

(defun send-stop-msg (port)
  (usocket:with-client-socket (sock nil
				    "127.0.0.1"
				    port
				    :protocol :datagram
				    :element-type '(unsigned-byte 8))
    (let ((stop-msg (osc:encode-message "stop")))
      (usocket:socket-send sock stop-msg (length stop-msg))
      (usocket:socket-close sock))))

(defparameter *width* 800)
(defparameter *height* 800)

(defparameter *scale* 0.2)
(defparameter *diffuse-r* 0.2)
(defparameter *diffuse-g* 0.4)
(defparameter *diffuse-b* 0.6)

(defparameter *prev-mouse-x* nil)
(defparameter *prev-mouse-y* nil)
(defparameter *angle-x* 0)
(defparameter *angle-y* 0)

(defclass my-window (glut:window)
  ()
  (:default-initargs :title "tea" :width *width* :height *height*
		     :mode '(:single :rgb :depth)))

;;modify https://github.com/3b/cl-opengl/blob/master/examples/misc/glut-teapot.lisp
(defmethod glut:display-window :before ((w my-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test))

(defmethod glut:display ((window my-window))
  (gl:load-identity)
  (gl:translate 0 0 -5)
  (gl:rotate 30 1 1 0)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse (list *diffuse-r* *diffuse-g* *diffuse-b* 0))
  (gl:clear :color-buffer :depth-buffer)
  (gl:color 1 1 1)
  (gl:front-face :cw)
  (gl:with-pushed-matrix
      (gl:rotate *angle-x* 1 0 0)
    (gl:rotate *angle-y* 0 1 0)
    (glut:solid-teapot *scale*))
  (gl:front-face :ccw)
  (gl:flush))

(defmethod glut:reshape ((window my-window) width height)
  (setf *width* width
	*height* height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 50 (/ width height) 0.5 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:mouse ((window my-window) button state x y)
  (if (and (eql button :left-button)
	   (eql state :down))
      (setf prev-mouse-x x
	    prev-mouse-y y)))

(defmethod glut:motion ((window my-window) x y)
  (let ((theta-y (* 360 (/ (- x prev-mouse-x) *width*)))
	(theta-x (* 360 (/ (- prev-mouse-y y) *height*))))
    (setf *angle-x* (- *angle-x* theta-x)
	  *angle-y* (+ *angle-y* theta-y)
	  *prev-mouse-x* x
	  *prev-mouse-y* y)))

(defmethod glut:idle ((widow my-window))
  (sleep (/ 1.0 10.0))
  (glut:post-redisplay))

(defmethod glut:close ((widow my-window))
  (send-stop-msg *port*))

(defun run ()
  (bt:make-thread #'(lambda () (start-osc-listen *port*))
		  :name "thread")
  (glut:display-window (make-instance 'my-window)))

(run)
