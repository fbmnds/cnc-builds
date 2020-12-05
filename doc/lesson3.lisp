(defclass Window (glut:window) 
  ()
  (:default-initargs
   :mode '(:rgba)))

;; Init
(defmethod glut:display-window :before ((window Window))
  (gl:clear-color 0 0 1 1))                  ; set blue

;; Display
(defmethod glut:display ((window Window))
  (gl:clear :color-buffer-bit)               ; clear buffer
                                        ; draw a square
  (gl:begin :line-loop)
  (%gl:vertex-2d -0.9 -0.9)
  (%gl:vertex-2d 0.9 -0.9)
  (%gl:vertex-2d 0.9 0.9)
  (%gl:vertex-2d -0.9 0.9)
  (gl:end)
  (gl:flush))                                ; show window

;; Main
(defun lesson3 ()
  (glut:display-window (make-instance 'Window)))  ; create window

(lesson3)
