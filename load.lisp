(require :asdf)
(asdf:load-asd "/home/dev/projects/cnc-builds/cnc-builds.asd")
(asdf:load-system :cnc-builds)

(paths/box-tests:run-tests)

(in-package paths/emitt)

(defparameter path (car paths/box-tests::tbox))
(defparameter tags
  (remove-if #'(lambda (c) (< (euklid (c- (car c) (cdr c))) 6.)) (group-2 path)))


(emitt-gcode-xy-z
                    (convert-path-dxyz% path tags 1. -0.5 5)
                    -0.5 1200)

(defparameter fn "file")

(with-open-file (f "~/Desktop/test2.nc"
                   :direction :output :if-exists :supersede)
  (format f "G21~%G91~%")
  (dolist (ln (emitt-gcode-xy-z
               (convert-path-dxyz% path tags 1. -1. 5) -0.5 1200))
    (format f "~a~%" ln)))


(with-open-file (f "~/projects/relays-ui/assets/data.csv"
                   :direction :output :if-exists :overwrite)
  (let ((i 0))
    (multiple-value-bind (v l-vec)
        (expand-path (car paths/box-tests::tbox)
                     '(((0.5 . 8.5) . (2.5 . 8.5))) 2 -1.5 5 2)
      (do ()
          ((= i l-vec))
        (if (> i 0)
            (format f "~%~3$;~3$;~3$"
                    (aref v i) (aref v (1+ i)) (aref v (+ i 2)))
            (format f "~3$;~3$;~3$"
                    (aref v i) (aref v (1+ i)) (aref v (+ i 2))))
        (setf i (+ i 3))))))


(with-open-file (f "~/projects/relays-ui/assets/data.csv"
                   :direction :output :if-exists :supersede)
  (let ((i 0))
    (multiple-value-bind (v l-vec)
        (expand-path (car paths/box-tests::tbox)
                     '(((2.5 . 2.5) . (10.5 . 2.5))) 2 -1.5 5 2)
      (do ()
          ((= i l-vec))
        (format f "~3$;~3$;~3$~%"
                    (aref v i) (aref v (1+ i)) (aref v (+ i 2)))
        (setf i (+ i 3))))))

(with-open-file (f "~/projects/relays-ui/assets/data.csv"
                   :direction :output :if-exists :supersede)
  (let* ((path (optimize-path (car paths/box-tests::tbox)))
         (tags (take-n-segments 4 (segments-by-length path)))
         (i 0))
    (multiple-value-bind (v l-vec)
        (expand-path path tags 2 -1.5 5 2)
      (do ()
          ((= i l-vec))
        (format f "~3$;~3$;~3$~%"
                (aref v i) (aref v (1+ i)) (aref v (+ i 2)))
        (setf i (+ i 3))))))


