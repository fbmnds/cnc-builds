;;
;; BoxMan walking
;; -----------------------------------------------------------------------------------------------
(defparameter *stepcycle* 400)  ; frame number for swing of arm
(defparameter *walkcycle* 4000) ; frame number for around the stage
(defvar *p* 1.0)                ; lookat
(defvar *flag* nil)             ; rotation flag
(defvar *toggle* t)             ; toggle switch
(defvar *frame* 0)              ; frames
(defvar *steps* 0)              ; step 
(defvar *walk* 0)               ; walk

(defun draw-figure (x y z vertex face)  
  (labels ((set-normal (n)
	     (gl:normal (aref n 0) (aref n 1) (aref n 2)))
	   (set-vertex (index)
             (let ((v (aref vertex index)))
	       (gl:vertex (* x (aref v 0))
			  (* y (aref v 1))
			  (* z (aref v 2)))))
           (draw-face (vertex-indices normal)
             (gl:begin :quads)
	       (set-normal normal)
               (map nil  #'set-vertex vertex-indices)
             (gl:end)))
    (map nil #'(lambda (x) (draw-face (first x) (second x))) face)))

(defun myBox (x y z)
  (let ((vertex #(#(-0.5  -1 -0.5)
	          #( 0.5  -1 -0.5)
		  #( 0.5 0.0 -0.5)
		  #(-0.5 0.0 -0.5)
		  #(-0.5  -1  0.5)
		  #( 0.5  -1  0.5)
		  #( 0.5 0.0  0.5)
		  #(-0.5 0.0  0.5)))
	(face '((#(0 1 2 3) #(0.0 0.0 -1.0))
		(#(1 5 6 2) #(1.0 0.0 0.0))
		(#(5 4 7 6) #(0.0 0.0 1.0))
		(#(4 0 3 7) #(-1.0 0.0 0.0))
		(#(4 5 1 0) #(0.0 -1.0 0.0))
		(#(3 2 6 7) #(0.0 1.0 0.0))))
        (red '(0.8 0.2 0.2 1.0))) 
    (gl:material :front :diffuse red)     ; red
    (draw-figure x y z vertex face)))

(defun armleg (girth length r1 r2)
  (gl:rotate r1 1.0 0.0 0.0)
  (myBox girth length girth)
  (gl:translate 0.0 (- -0.05 length) 0.0)
  (gl:rotate r2 1.0 0.0 0.0)
  (myBox girth length girth)) 
  
(defun myGround (height)
  (let ((ground #((0.6 0.6 0.6 1.0) (0.3 0.3 0.3 1.0)))
    	        (toggle 0)
                (j -5)
                (i -5))
    (labels ((switch ()
      (cond ((= toggle 0) (setf toggle 1))
            ((= toggle 1) (setf toggle 0))))) 
    (gl:begin :quads)	
      (gl:normal 0.0 1.0 0.0)
      (dotimes (m 10)
    	(dotimes (n 10)
    	  (gl:material :front :diffuse  (aref ground toggle))
          (gl:vertex i height j)
    	  (gl:vertex i height (+ j 1))
          (gl:vertex (+ i 1) height (+ j 1))
          (gl:vertex (+ i 1) height j)
	  (switch)     ; change toggle
          (incf i)
    	  (when (= i 5) 
    	    (setf i -5)
	    (switch))) ; change toggle	    
    	(incf j))
    (gl:end)))) 	

;; Set window class
(defclass Window (glut:window)
  ((fullscreen :initarg :fullscreen :reader fullscreen-p))
  (:default-initargs
   :pos-x 100 :pos-y 100 :width 500 :height 500
   :fullscreen nil
   :mode '(:double :rgba) :title "BoxMan"))

;; Init
(defmethod glut:display-window :before ((window Window))
  (gl:clear-color 1.0 1.0 1.0 1.0)  ; set white
  (gl:enable :depth-test)           ; enable depth testing
  (gl:cull-face :front)             ; cullface
  (gl:enable :lighting :light0)     ; light0
  (when (fullscreen-p window)       ; check to see if fullscreen needed
    (glut:full-screen)))            ; if so, then tell GLUT

;; View port
(defmethod glut:reshape ((window Window) width height)
  (gl:viewport 0 0 width height)    ; make the whole window to viewport
  (gl:matrix-mode :projection)      ; specifying the perspective transformation matrix
  (gl:load-identity)                ; initialization of perspective transformation matrix
  (glu:perspective 30.0 (/ width height) 1.0 100.0)
  (gl:matrix-mode :modelview))      ; specification of the model view transformation matrix

;; idle
(defmethod glut:idle ((window Window))
  ;; user idling process
  (when (eql *flag* t)
  ; (sleep (/ 1.0 60.0))
    (glut:post-redisplay)))

;; mouse
(defmethod glut:mouse ((window Window) button state x y)
  (declare (ignore x y))
  (case button 
    (:LEFT-BUTTON
      (if (eql state :DOWN)
	(progn  
          (setf *flag* t)
	  (glut:enable-event window :idle))))
    (:MIDDLE-BUTTON
      (if (eql state :DOWN)
        (progn
	  (setf *flag* nil)
          (glut:disable-event window :idle))))
    (:RIGHT-BUTTON
      (if (eql state :DOWN)
	(glut:post-redisplay)))))

;; mouse-wheel
(defmethod glut:mouse-wheel ((window Window) wheel direction x y)
 (declare (ignore x y))
 (case direction
   (:wheel-down
     (progn
       (incf *p* 0.2) 
       (when (>= *p* 9.8)
	 (setf *p* 9.8))
       (glut:post-redisplay)))       
   (otherwise
     (progn 
       (decf *p* 0.2)
       (when (<= *p* 0.2)
         (setf *p* 0.2))
       (glut:post-redisplay)))))

;; Keyboard
(defmethod glut:keyboard ((window Window) key x y)
  (declare (ignore x y))
    (case key
      (#\ESC                               ; ESC --> exit
        (progn 
	  (setf *toggle* t)
        (glut:destroy-current-window)))
      (#\f                                 ; f   --> full or windowed
        (if (eql *toggle* t)
	    (progn
	      (setf *toggle* nil)
	      (glut:full-screen))
	    (progn
	      (setf *toggle* t)
	      (glut:position-window 100 100)       ; initial position
              (glut:reshape-window 500 500))))))   ; initial width height

;; Display
(defmethod glut:display ((window Window))  
  (let ((lightpos '(3.0 4.0 5.0 1.0))
	; BoxMan parameter
	(ll1 0.0)  ; left  leg crotch   angle
	(ll2 0.0)  ; left  leg knee     angle
	(rl1 0.0)  ; right leg crotch   angle 
	(rl2 0.0)  ; right leg knee     angle
	(la1 0.0)  ; left  arm shoulder angle
	(la2 0.0)  ; left  arm elbow    angle
	(ra1 0.0)  ; right arm shoulder angle
	(ra2 0.0)  ; right arm elbow    angle
	(px 0.0)   ; position x
	(pz 0.0)   ; position z
	(r 0.0)    ; direction
	(h 0.0))   ; height from ground

    (setf *steps* (/ (mod *frame* *stepcycle*) *stepcycle*))  ; from 0 to 1 
    (setf *walk*  (/ (mod *frame* *walkcycle*) *walkcycle*))  ; from 0 to 1
 
    ; BoxMan direction
    (setf r (- (* 360 *walk*)))
    ; BoxMan position
    (setf px (* (cos (* (* 2 pi) *walk*)) 3))
    (setf pz (* (sin (* (* 2 pi) *walk*)) 3))

    ; BoxMan legs swing
    (setf ll1 (* 25 (sin (* (/ (* 360 *steps*) 360) (* 2 pi)))))
    (setf ll2 (- 25 (* 25 (cos (* (/ (* 360 *steps*) 360) (* 2 pi))))))
    (setf rl1 (* 25 (cos (* (/ (+ (* 360 *steps*) 30) 360) (* 2 pi)))))
    (setf rl2 (- 25 (* 25 (sin (* (/ (+ (* 360 *steps*) 30) 360) (* 2 pi))))))
    ; BoxMan arms swing
    (setf la1 (* 25 (cos (* (/ (+ (* 360 *steps*) 30) 360) (* 2 pi)))))
    (setf la2 (- -25 (* 25 (sin (* (/ (+ (* 360 *steps*) 30) 360) (* 2 pi))))))
    (setf ra1 (* 25 (sin (* (/ (* 360 *steps*) 360) (* 2 pi)))))
    (setf ra2 (- -25 (* 25 (cos (* (/ (* 360 *steps*) 360) (* 2 pi))))))

    ; frame count
    (incf *frame*)
    
    (gl:clear :color-buffer-bit :depth-buffer-bit)   ; clear buffer
    (gl:load-identity)
    (gl:light :light0 :position lightpos)
    (glu:look-at 0.0 0.0 *p* 0.0 0.0 0.0 0.0 1.0 0.0)
    (gl:translate 0.0 0.0 -10.0)
    ; ground
    (myGround -1.8)
    ; position and direction
    (gl:translate px  h  pz)
    (gl:rotate r 0.0 1.0 0.0)    ; BoxMan's direction
    ; head
    (myBox 0.20 0.25 0.22)
    ; body
    (gl:translate 0.0 -0.3 0.0)
    (myBox 0.4 0.6 0.3)
    ; left leg
    (gl:push-matrix)
    (gl:translate 0.1 -0.65 0.0)
    (armleg 0.2 0.4 ll1 ll2)
    (gl:pop-matrix)
    ; right leg
    (gl:push-matrix)
    (gl:translate -0.1 -0.65 0.0)
    (armleg 0.2 0.4 rl1 rl2)
    (gl:pop-matrix)
    ; left arm
    (gl:push-matrix)
    (gl:translate 0.28 0.0 0.0)
    (armleg 0.16 0.4 la1 la2)
    (gl:pop-matrix)
    ; right arm
    (gl:push-matrix)
    (gl:translate -0.28 0.0 0.0)
    (armleg 0.16 0.4 ra1 ra2)
    (gl:pop-matrix)
    
    (glut:swap-buffers)))
 
;; Main
(defun BoxMan ()
  (glut:display-window (make-instance 'Window)))  ; create window

(BoxMan)
