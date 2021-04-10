(require :asdf)
(asdf:load-asd "/home/dev/projects/cnc-builds/cnc-builds.asd")
(asdf:load-system :cnc-builds)

(paths/box-tests:run-tests)

;;(in-package paths/emitt)
;;(defparameter path (car paths/box-tests::tbox))

;;(cons (cons 2.5 10.5) 0.)


