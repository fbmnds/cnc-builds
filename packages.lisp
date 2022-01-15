
(defpackage #:paths
  (:use #:cl)
  (:export ;; paths
           #:set-precision
           #:set-machine-precision
           #:round*
           #:zerop*
           #:xy-p
           #:xyz-p
           #:coord
           #:coord-xy
           #:coord-xyz
           #:list-xy-p
           #:list-xyz-p
           #:path
           #:path-xy
           #:path-xyz
           #:xy-xy-p
           #:xyz-xyz-p
           #:segment
           #:segment-xy
           #:segment-xyz
           #:list-xy-xy-p
           #:list-xyz-xyz-p
           #:path-segments
           #:path-segments-xy
           #:path-segments-xyz
           #:c-x
           #:c-y
           #:c-z
           #:test-c-z
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
           #:collinear-coord-p
           #:collinear-segment-p
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
           #:shift-path-origin
           #:rad->deg
           #:deg->rad
           #:polar->cartesian
           #:cartesian->polar
           #:radian
           #:n-d-polar
           #:spiral-polar
           #:spiral
           #:closed-spiral
           #:while
           ;; emitt
           #:group
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
           #:optimize-microsteps
           #:optimize-path
           #:segments-by-length
           #:inner-ticks
           #:tag-path
           #:expand-path
           #:geometric-center
           #:inner-path
           #:outer-path
           #:fill-pocket
           #:fill-inner-rectangle
           ;; box
           #:round_d-3
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

(defpackage #:paths/tests
  (:use #:cl #:paths #:paths/view)
  (:export #:run-tests
           #:run-view-tests))

