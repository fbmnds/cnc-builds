;;;; cnc-host.asd

(asdf:defsystem #:cnc-builds
  :description "Describe cnc-builds here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on
  (#:asdf
   #:quicklisp
   #:cl-opengl
   #:cl-glut
   #:cl-glu
   ;;#:cl-glfw3
   )
  :serial t
  :components
  ((:file "packages")
   (:module "paths"
    :components
    ((:file "emitt")
     (:file "box")
     (:file "paths")))
   (:module "views"
    :components
    ((:file "view")))
   (:module "t"
    :components
    ((:file "box-tests")))))

