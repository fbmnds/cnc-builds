(require :asdf)
(asdf:load-asd "/home/dev/projects/cnc-builds/cnc-builds.asd")
(asdf:load-system :cnc-builds)

(paths/box-tests:run-tests)

(in-package paths/emitt)

(defparameter path (car paths/box-tests::tbox))
(remove-if #'(lambda (c) (< (euklid (c- (car c) (cdr c))) 6.)) (group-2 path))



