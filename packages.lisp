
(defpackage #:paths/box
  (:use #:cl)
  (:export #:round_d-3
           #:eql-c
           #:eql-l
           #:eql_d-3
           #:flip-x
           #:flip-y
           #:flip-45
           #:shift-x
           #:shift-y
           #:shift-flip
           #:shift-to-llc
           #:cut-bd
           #:cut-ac
           #:box-var-inner
           #:box-var-outer-1
           #:box-var-outer-2
           #:box
           #:box-cut-z
           #:emitt
           #:emitt-box))

(defpackage #:paths
  (:use #:cl)
  (:export #:group
           #:with-type
           #:c-
           #:c+
           #:c*
           #:euklid
           #:c=
           #:c-normed
           #:normale-+
           #:normale--
           #:det2
           #:shift-corner-+
           #:shift-corner--
           #:shift-path-+
           #:shift-path--))

(defpackage #:paths/view
  (:use #:cl)
  (:local-nicknames (#:gl #:cl-opengl)
                    (#:glut #:cl-glut))
  (:export #:view
           #:multi-view))

(defpackage #:paths/box-tests
  (:use #:cl #:paths #:paths/box #:paths/view)
  (:export #:run-tests))

