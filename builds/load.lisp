(require :asdf)
(asdf:load-asd "/home/dev/projects/cnc-builds/cnc-builds.asd")
(asdf:load-system :cnc-builds)

(paths/box-tests:run-tests)


;;(in-package #:paths/box-tests)

;;(in-package paths/emitt)

(defparameter d1-ac 12)
(defparameter d2-ac 12)
(defparameter d1-bd 12)
(defparameter d2-bd 12)
(defparameter dy  10)
(defparameter eps 0.01)

(defparameter lx 150)
(defparameter ly 260)
(defparameter lz 200)
;;(defparameter h (* 0.8 lz))
(defparameter spacer-d 6)

(defparameter tbox
  (paths/box:box lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-d))

(paths/view:multi-view tbox)

(defparameter fn "/home/dev/projects/cnc-builds/builds/tbox.scad")

(with-open-file (f fn
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :overwrite)
  (format f "~a~%" (paths/emitt:emitt-scad-box tbox)))


