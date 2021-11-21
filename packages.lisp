
(defpackage #:paths
  (:use #:cl)
  (:export #:*precision*
           #:round*
           #:zerop*
           #:xy-p
           #:xyz-p
           #:coord
           #:coord-xy
           #:coord-xyz
           #:c-x
           #:c-y
           #:c-z
           #:c1-c2=
           #:group-2
           #:group-3
           #:with-type
           #:c-
           #:c2-c1-
           #:c+
           #:c*
           #:euklid
           #:c=
           #:c-normed
           #:normale-+
           #:normale--
           #:det2
           #:collinear-2d
           #:shift-corner-+
           #:shift-corner--
           #:shift-path-+
           #:shift-path--
           #:in-row-p
           #:stats
           #:stats-acc
           #:trim-path
           #:circle-path
           #:linear-solution
           #:radius-center-in-corner
           #:min-xy-path
           #:flip-path
           #:shift-path-origin))

(defpackage #:paths/emitt
  (:use #:cl #:paths)
  (:export  #:group
            #:group-2
            #:emitt-scad
            #:emitt-scad-box
            #:emitt-gcode-path
            #:emitt-gcode
            #:insert-tag
            #:insert-tags
            #:convert-dxyz
            #:optimize-relative-distances
            #:convert-path-dxyz
            #:convert-path-dxyz%
            #:emitt-gcode-xy-z
            #:close-path
            #:optimize-path
            #:segments-by-length
            #:inner-ticks
            #:tag-path
            #:expand-path))

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
           #:box-cut-z))

(defpackage #:paths/view
  (:use #:cl)
  (:local-nicknames (#:gl #:cl-opengl)
                    (#:glut #:cl-glut)
                    (#:paths #:paths))
  (:export #:view
           #:multi-view
           #:colored-multi-view))

(defpackage #:paths/box-tests
  (:use #:cl #:paths #:paths/box #:paths/emitt #:paths/view)
  (:export #:run-tests
           #:run-view-tests))

