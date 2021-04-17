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
                   :direction :output :if-exists :overwrite)
  (format f "G21~%G91~%")
  (dolist (ln (emitt-gcode-xy-z
               (convert-path-dxyz% path tags 1. -1. 5) -0.5 1200))
    (format f "~a~%" ln)))
