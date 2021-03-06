
(in-package #:paths/tests)

(defparameter l1 '((1 . 1) (11 . 1) (11 . 5) (1 . 5)))
(defparameter l2 '((0 . 0) (10 . 0) (10 . 5) (15 . 5)))

(defparameter d1-ac 4)
(defparameter d2-ac 4)
(defparameter d1-bd 4)
(defparameter d2-bd 4)
(defparameter dy  2.5)
(defparameter eps 0.01)

(defparameter lx (+ 60 (* 2 dy)))
(defparameter ly (+ 40 (* 2 dy)))
(defparameter lz (+ 20 (* 2 dy)))
(defparameter h (* 0.8 lz))
(defparameter spacer-d 4)

(defparameter tbox
  (box lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-d))

(defparameter tbox-cut-z
  (box-cut-z h lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-d))

(defparameter e-box (emitt-scad-box tbox))

(defparameter square '((0. . 0.) (10. . 0.) (10. . 10.) (0. . 10.)))

(defparameter rhomb '((0 . 10) (10 . 0) (20 . 10) (10 . 20)))

(defparameter spiral-01
  (nconc
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (closed-spiral 01 (cons 50 0) (cons 60 0)))
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (spiral 01 (cons 50 0) (cons 60 0)))
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (spiral 01 (cons 40 0) (cons 50 0)))
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (spiral 01 (cons 30 0) (cons 40 0)))
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (spiral 01 (cons 20 0) (cons 30 0)))
   (mapcar #'(lambda (c) (cons (+ 60 (c-x c))(+ 60 (c-y c))))
           (spiral 01 (cons 0 0) (cons 20 0)))))

(defun run-tests ()
  (assert (equal (flip-x l1) '((1 . -1) (11 . -1) (11 . -5) (1 . -5))))
  (assert (equal (flip-y l1) '((-1 . 1) (-11 . 1) (-11 . 5) (-1 . 5))))
  (assert (equal (flip-45 l1) '((1 . 1) (1 . 11) (5 . 11) (5 . 1))))

  (assert (equal (shift-x 1 l1) '((2 . 1) (12 . 1) (12 . 5) (2 . 5))))
  (assert (equal (shift-y 1 l1) '((1 . 2) (11 . 2) (11 . 6) (1 . 6))))
  (assert (equal (shift-flip 5 l2) '((0 . 5) (10 . 5) (10 . 0) (15 . 0))))

  (assert (eql_d-3 l1 l1))
  (assert (eql_d-3 l2 l2))
  (assert (not (eql_d-3 l1 l2)))
  
  (assert (eql_d-3 (flip-x l1) '((1 . -1) (11 . -1) (11 . -5) (1 . -5))))
  (assert (eql_d-3 (flip-y l1) '((-1 . 1) (-11 . 1) (-11 . 5) (-1 . 5))))
  (assert (eql_d-3 (flip-45 l1) '((1 . 1) (1 . 11) (5 . 11) (5 . 1))))

  (assert (eql_d-3 (shift-x 1 l1) '((2 . 1) (12 . 1) (12 . 5) (2 . 5))))
  (assert (eql_d-3 (shift-y 1 l1) '((1 . 2) (11 . 2) (11 . 6) (1 . 6))))
  (assert (eql_d-3 (shift-flip 5 l2) '((0 . 5) (10 . 5) (10 . 0) (15 . 0))))

  (assert (eql_d-3 l1 (shift-to-llc '((1 . 5) (1 . 1) (11 . 1) (11 . 5)))))

  (assert (in-row-p '((2.5 . 2.5) (9.5 . 2.5) (9.5 . 2.5))))
  (assert (in-row-p '((9.5 . 2.5) (9.5 . 2.5) (9.5 . 0.0))))
  (assert (not (in-row-p '((2.5 . 2.5) (9.5 . 2.5) (9.5 . 0.0)))))

  (assert (equal (stats (car tbox))
                 '(:LEN 90
                   :MAX-X 65.0 :MIN-X 0.0 :AVG-X 33.077778
                   :MAX-Y 45.0 :MIN-Y 0.0 :AVG-Y 22.9)))
  (assert (equal (stats-acc (list (car tbox)))
                 '(:LEN 90
                   :MAX-X 65.0 :MIN-X 0.0 :AVG-X 33.077778
                   :MAX-Y 45.0 :MIN-Y 0.0 :AVG-Y 22.9)))
  (assert (equal (stats-acc tbox)
                 '(:LEN 436
                   :MAX-X 192.0 :MIN-X 0.0 :AVG-X 92.70642
                   :MAX-Y 74.0 :MIN-Y 0.0 :AVG-Y 35.766056)))

  (assert (c= (geometric-center '((0 . 0) (1 . 0) (1 . 1)))
              '(0.666667 . 0.333333)))
  (assert (c= (geometric-center '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
              '(0.5 . 0.5)))
  (assert (c= (geometric-center '((0 . 0) (2 . 0) (3 . 1) (1 . 1)))
              '(1.5 . 0.5)))
  (assert (c= (geometric-center (circle-path 5 12))
              '(0.0 . 0.0)))

  (let ((x '(1 2 3 4 5)))
    (assert (equal (group-3 x)
                   '((1 2 3) (2 3 4) (3 4 5) (4 5 1) (5 1 2))))
    (setf x '(1 2 3 4 5 6))
    (assert (equal (group-3 x)
                   '((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 1) (6 1 2))))
    (setf x '((2.5 . 2.5) (9.5 . 2.5) (9.5 . 0)))
    (assert (equal (shift-corner-+ 1.5 (car x) (cadr x) (caddr x))
                   '(11.0 . 4.0))))

  (let ((c1-c2 (cons (cons 0. 0.) (cons 20. 0.)))
        (ret '(((0. . 0.) 8.0 . 0.0)
               (:TAG (8.0 . 0.0) 12.0 . 0.0)
               ((12.0 . 0.0) 20 . 0.))))
    (assert (reduce #'(lambda (x y) (and x y))
                    (mapcar #'equal ret (insert-tag c1-c2 2)))))
  
  (let ((c1-c2 (cons (cons 0. 0.) (cons 20. 10.)))
        (ret '(((0 . 0) 8.211146 . 4.105572)
               (:tag (8.211146 . 4.105572) 11.788854 . 5.894428)
               ((11.788854 . 5.894428) 20 . 10))))
    (assert (reduce #'(lambda (x y) (and x y))
                    (mapcar #'(lambda (x y)
                                (if (equal :tag (car x))
                                    (c1-c2= (cdr x) (cdr y))
                                    (c1-c2= x y)))
                            ret (insert-tag c1-c2 2))))
    (assert (c= 4. (euklid (c- (cadadr ret) (cddadr ret))))))
  
  (let ((ret '(((0 . 0) 2.5 . 0.0)
               (:TAG (2.5 . 0.0) 7.5 . 0.0)
               ((7.5 . 0.0) 10 . 0)
               ;;
               ((10 . 0) 10.0 . 2.5)
               (:TAG (10.0 . 2.5) 10.0 . 7.5)
               ((10.0 . 7.5) 10 . 10)
               ;;
               ((10 . 10) 7.5 . 10.0)
               (:TAG (7.5 . 10.0) 2.5 . 10.0)
               ((2.5 . 10.0) 0 . 10)
               ;;
               ((0 . 10) 0.0 . 7.5)
               (:TAG (0.0 . 7.5) 0.0 . 2.5)
               ((0.0 . 2.5) 0 . 0))))
    (assert (equal ret (insert-tags square (group-2 square) 2.5))))
  
  (flet ((convert-dxyz- (c1-c2) (convert-dxyz c1-c2 1 0.5 5)))
    (let ((ret '((((2.5 . 0.0) . 0.))
                 (((5.0 . 0.0) . 0.))
                 (((2.5 . 0.0) . 0.))
                 (((0.0 . 2.5) . 0.))
                 (((0.0 . 5.0) . 0.))
                 (((0.0 . 2.5) . 0.))
                 (((-2.5 . 0.0) . 0.))
                 (((-5.0 . 0.0) . 0.))
                 (((-2.5 . 0.0) . 0.))
                 (((0.0 . -2.5) . 0.))
                 (((0.0 . -5.0) . 0.))
                 (((0.0 . -2.5) . 0.)))))
      (assert (equal ret
                     (mapcar #'convert-dxyz-
                             (insert-tags square (group-2 square) 2.5))))))
  
  (flet ((convert-dxyz- (c1-c2) (convert-dxyz c1-c2 4 0.5 5)))
    (let ((ret '((((2.5 . 0.0) . 0.))
                 (((0 . 0) . -2.0)
                  ((5.0 . 0.0) . 0)
                  ((0 . 0) . 2.0))
                 (((2.5 . 0.0) . 0.))
                 (((0.0 . 2.5) . 0.))
                 (((0 . 0) . -2.0)
                  ((0.0 . 5.0) . 0)
                  ((0 . 0) . 2.0))
                 (((0.0 . 2.5) . 0.))
                 (((-2.5 . 0.0) . 0.))
                 (((0 . 0) . -2.0)
                  ((-5.0 . 0.0) . 0)
                  ((0 . 0) . 2.0))
                 (((-2.5 . 0.0) . 0.))
                 (((0.0 . -2.5) . 0.))
                 (((0 . 0) . -2.0)
                  ((0.0 . -5.0) . 0)
                  ((0 . 0) . 2.0))
                 (((0.0 . -2.5) . 0.)))))
      (assert (equal ret
                     (mapcar #'convert-dxyz-
                             (insert-tags square (group-2 square) 2.5))))))


  
  
  (let ((ret '(((0 . 0) . 0.5)
               ((2.5 . 0.0) . 0) ((5.0 . 0.0) . 0) ((2.5 . 0.0) . 0)
               ((0.0 . 2.5) . 0) ((0.0 . 5.0) . 0) ((0.0 . 2.5) . 0)
               ((-2.5 . 0.0) . 0) ((-5.0 . 0.0) . 0) ((-2.5 . 0.0) . 0)
               ((0.0 . -2.5) . 0) ((0.0 . -5.0) . 0) ((0.0 . -2.5) . 0)

               ((0 . 0) . 0.5)
               ((2.5 . 0.0) . 0) ((5.0 . 0.0) . 0) ((2.5 . 0.0) . 0)
               ((0.0 . 2.5) . 0) ((0.0 . 5.0) . 0) ((0.0 . 2.5) . 0)
               ((-2.5 . 0.0) . 0) ((-5.0 . 0.0) . 0) ((-2.5 . 0.0) . 0)
               ((0.0 . -2.5) . 0) ((0.0 . -5.0) . 0) ((0.0 . -2.5) . 0)

               ((0 . 0) . 0.5)
               ((2.5 . 0.0) . 0)
               ((0 . 0) . -1.5) ((5.0 . 0.0) . 0) ((0 . 0) . 1.5)
               ((2.5 . 0.0) . 0) ((0.0 . 2.5) . 0)
               ((0 . 0) . -1.5) ((0.0 . 5.0) . 0) ((0 . 0) . 1.5)
               ((0.0 . 2.5) . 0) ((-2.5 . 0.0) . 0)
               ((0 . 0) . -1.5) ((-5.0 . 0.0) . 0) ((0 . 0) . 1.5)
               ((-2.5 . 0.0) . 0) ((0.0 . -2.5) . 0)
               ((0 . 0) . -1.5) ((0.0 . -5.0) . 0) ((0 . 0) . 1.5)
               ((0.0 . -2.5) . 0)

               ((0 . 0) . 0.5)
               ((2.5 . 0.0) . 0)
               ((0 . 0) . -2.0) ((5.0 . 0.0) . 0) ((0 . 0) . 2.0)
               ((2.5 . 0.0) . 0) ((0.0 . 2.5) . 0)
               ((0 . 0) . -2.0) ((0.0 . 5.0) . 0) ((0 . 0) . 2.0)
               ((0.0 . 2.5) . 0) ((-2.5 . 0.0) . 0)
               ((0 . 0) . -2.0) ((-5.0 . 0.0) . 0) ((0 . 0) . 2.0)
               ((-2.5 . 0.0) . 0) ((0.0 . -2.5) . 0)
               ((0 . 0) . -2.0) ((0.0 . -5.0) . 0) ((0 . 0) . 2.0)
               ((0.0 . -2.5) . 0))))
    (assert (equal ret
                   (convert-path-dxyz%
                    square (group-2 square) 2.5 0.5 5))))
  
  (let ((ret '(((0. . 0.) . 0.5)
               ((10.0 . 0) . 0.)
               ((0 . 10.0) . 0.)
               ((-10.0 . 0) . 0.)
               ((0 . -10.0) . 0.)

               ((0. . 0.) . 0.5)
               ((10.0 . 0) . 0.)
               ((0 . 10.0) . 0.)
               ((-10.0 . 0) . 0.)
               ((0 . -10.0) . 0.)

               ((0. . 0.) . 0.5)
               ((2.5 . 0.0) . 0.)
               ((0.0 . 0.0) . -1.5) ((5.0 . 0.0) . 0.0) ((0.0 . 0.0) . 1.5)
               ((2.5 . 0.0) . 0.) ((0.0 . 2.5) . 0.)
               ((0.0 . 0.0) . -1.5) ((0.0 . 5.0) . 0.0) ((0.0 . 0.0) . 1.5)
               ((0.0 . 2.5) . 0.) ((-2.5 . 0.0) . 0.)
               ((0.0 . 0.0) . -1.5) ((-5.0 . 0.0) . 0.0) ((0.0 . 0.0) . 1.5)
               ((-2.5 . 0.0) . 0.) ((0.0 . -2.5) . 0.)
               ((0.0 . 0.0) . -1.5) ((0.0 . -5.0) . 0.0) ((0.0 . 0.0) . 1.5)
               ((0.0 . -2.5) . 0.)

               ((0. . 0.) . 0.5)
               ((2.5 . 0.0) . 0.)
               ((0.0 . 0.0) . -2.0) ((5.0 . 0.0) . 0.0) ((0.0 . 0.0) . 2.0)
               ((2.5 . 0.0) . 0.) ((0.0 . 2.5) . 0.)
               ((0.0 . 0.0) . -2.0) ((0.0 . 5.0) . 0.0) ((0.0 . 0.0) . 2.0)
               ((0.0 . 2.5) . 0.) ((-2.5 . 0.0) . 0.)
               ((0.0 . 0.0) . -2.0) ((-5.0 . 0.0) . 0.0) ((0.0 . 0.0) . 2.0)
               ((-2.5 . 0.0) . 0.) ((0.0 . -2.5) . 0.)
               ((0.0 . 0.0) . -2.0) ((0.0 . -5.0) . 0.0) ((0.0 . 0.0) . 2.0)))
        (gc '("G0 Z-0.500 F960.000"
              "G0 X10.000 F1200.000" "G0 Y10.000" "G0 X-10.000" "G0 Y-10.000"
              "G0 Z-0.500 F960.000"
              "G0 X10.000 F1200.000" "G0 Y10.000" "G0 X-10.000" "G0 Y-10.000"
              "G0 Z-0.500 F960.000"
              "G0 X2.500 F1200.000"
              "G0 Z1.500 F960.000" "G0 X5.000" "G0 Z-1.500"
              "G0 X2.500 F1200.000" "G0 Y2.500"
              "G0 Z1.500 F960.000" "G0 Y5.000" "G0 Z-1.500"
              "G0 Y2.500 F1200.000" "G0 X-2.500"
              "G0 Z1.500 F960.000" "G0 X-5.000" "G0 Z-1.500"
              "G0 X-2.500 F1200.000" "G0 Y-2.500"
              "G0 Z1.500 F960.000" "G0 Y-5.000" "G0 Z-1.500"
              "G0 Y-2.500 F1200.000"
              "G0 Z-0.500 F960.000"
              "G0 X2.500 F1200.000"
              "G0 Z2.000 F960.000" "G0 X5.000" "G0 Z-2.000"
              "G0 X2.500 F1200.000" "G0 Y2.500"
              "G0 Z2.000 F960.000" "G0 Y5.000" "G0 Z-2.000"
              "G0 Y2.500 F1200.000" "G0 X-2.500"
              "G0 Z2.000 F960.000" "G0 X-5.000" "G0 Z-2.000"
              "G0 X-2.500 F1200.000" "G0 Y-2.500"
              "G0 Z2.000 F960.000" "G0 Y-5.000" "G0 Z-2.000")))
    (assert (c= ret
                (convert-path-dxyz square (group-2 square) 2.5 0.5 5)))
    (assert (equal gc
                   (emitt-gcode-xy-z
                    (convert-path-dxyz square (group-2 square) 2.5 -0.5 5)
                    -0.5 1200))))
  (assert (equal (stats spiral-01)
                 '(:LEN 1799
                   :MAX-X 120.0 :MIN-X 0.0020939999 :AVG-X 60.15845
                   :MAX-Y 119.99946 :MIN-Y 5.42E-4 :AVG-Y 58.692585)))
  (assert (c= (geometric-center spiral-01)
              '(59.979214 . 58.75419)))
  (let* ((path '((0 . 0) (250 . 0) (250 . 170) (0 . 170)))
         (outer-path (close-path (shift-path-- 2 path)))
         (path-increments (path-to-increments outer-path)))
    (assert (equal outer-path
                   '((252.0 . -2.0) (252.0 . 172.0)
                     (-2.0 . 172.0) (-2.0 . -2.0)
                     (252.0 . -2.0))))
    (assert (equal path-increments
                   '(((0.0 . 174.0) . 0.0)
                     ((-254.0 . 0.0) . 0.0) ((0.0 . -174.0) . 0.0)
                     ((254.0 . 0.0) . 0.0))))
    ;;(paths/view:colored-multi-view
    ;; (list (cons :white path) (cons :green outer-path))

    (assert (equal (emitt-gcode-xyz outer-path 1200)
                   '("G0 Y174.000 F1200"
                     "G0 X-254.000"
                     "G0 Y-174.000"
                     "G0 X254.000")))
    (assert (equal (emitt-gcode-xyz-from-zero outer-path 1200)
                   '(;; move from ZERO
                     "G0G90 Z5.000 F960"
                     "G0 X252.000Y-2.000 F1200"
                     "G0G91 Z-5.000 F960"
                     ;; follow PATH
                     "G0 Y174.000 F1200"
                     "G0 X-254.000"
                     "G0 Y-174.000"
                     "G0 X254.000"))))

  (assert
   (equal
    (path-to-increments
     '(((-2 . -2) . 0)
       ((-2 . -2) . -10)
       ((252 . -2) . -10) ((252 . 174) . -10)
       ((-2 . 174) . -10) ((-2 . -2) . -10)
       ((-2 . -2) . -20)
       ((252 . -2) . -20) ((252 . 174) . -20)
       ((-2 . 174) . -20) ((-2 . -2) . -20)))
    '(((0.0 . 0.0) . -10.0)
      ((254.0 . 0.0) . 0.0) ((0.0 . 176.0) . 0.0)
      ((-254.0 . 0.0) . 0.0) ((0.0 . -176.0) . 0.0)
      ((0.0 . 0.0) . -10.0)
      ((254.0 . 0.0) . 0.0) ((0.0 . 176.0) . 0.0)
      ((-254.0 . 0.0) . 0.0) ((0.0 . -176.0) . 0.0))))

  (assert
   (equal
    (emitt-gcode-xyz '(((-2 . -2) . 0)
                       ((-2 . -2) . -10)
                       ((252 . -2) . -10) ((252 . 174) . -10)
                       ((-2 . 174) . -10) ((-2 . -2) . -10)
                       ((-2 . -2) . -20)
                       ((252 . -2) . -20) ((252 . 174) . -20)
                       ((-2 . 174) . -20) ((-2 . -2) . -20))
                     1200)
    '("G0 Z-10.000 F960"
      "G0 X254.000 F1200" "G0 Y176.000"
      "G0 X-254.000" "G0 Y-176.000"
      "G0 Z-10.000 F960"
      "G0 X254.000 F1200" "G0 Y176.000"
      "G0 X-254.000" "G0 Y-176.000")))

  (assert
   (equal
    (emitt-gcode-xyz-from-zero
     '(((-2 . -2) . 0)
       ((-2 . -2) . -10)
       ((252 . -2) . -10) ((252 . 174) . -10)
       ((-2 . 174) . -10) ((-2 . -2) . -10)
       ((-2 . -2) . -20)
       ((252 . -2) . -20) ((252 . 174) . -20)
       ((-2 . 174) . -20) ((-2 . -2) . -20))
     1200)
    '("G0G90 Z5.000 F960"
      "G0 X-2.000Y-2.000 F1200"
      "G0G91 Z-5.000 F960"
      "G0 Z-10.000 F960"
      "G0 X254.000 F1200" "G0 Y176.000"
      "G0 X-254.000" "G0 Y-176.000"
      "G0 Z-10.000 F960"
      "G0 X254.000 F1200" "G0 Y176.000"
      "G0 X-254.000" "G0 Y-176.000"))))

(defun run-view-tests ()
  (paths/view:view (car tbox) 100 100)
  (paths/view:multi-view tbox 200 200)
  (paths/view:multi-view
   (list (remove-if #'null
                    (shift-path-- 1.5 (car tbox)))
         (car tbox)))
  (paths/view:multi-view
   (list (remove-if #'null
                    (shift-path-- 1.5 (caddr tbox)))
         (caddr tbox)))
  (paths/view:multi-view
   (append tbox
           (mapcar #'(lambda (l)
                       (remove-if #'null (shift-path-- 1.5 l)))
                   tbox)
           (mapcar #'(lambda (l)
                       (remove-if #'null (shift-path-+ 1.5 l)))
                   tbox)))
  (paths/view:colored-multi-view
   (append (mapcar #'(lambda (p) (cons :white p)) tbox)
           (mapcar #'(lambda (p) (cons :red p))
                   (mapcar #'(lambda (l) (shift-path-- 1.5 l)) tbox))
           (mapcar #'(lambda (p) (cons :green p))
                   (mapcar #'(lambda (l) (shift-path-+ 1.5 l)) tbox))))
  (paths/view:colored-multi-view
   (append (mapcar #'(lambda (p) (cons :white p))
                   tbox-cut-z)
           (mapcar #'(lambda (p) (cons :red p))
                   (mapcar #'(lambda (l) (shift-path-- 1.5 l))
                           tbox-cut-z))
           (mapcar #'(lambda (p) (cons :green p))
                   (mapcar #'(lambda (l) (shift-path-+ 1.5 l))
                           tbox-cut-z))))
  (paths/view:colored-multi-view
   (list (cons :white rhomb)
         (cons :red (shift-path-- 1.5 rhomb))
         (cons :green (shift-path-+ 1.5 rhomb))))
  (paths/view:colored-multi-view
   (list (cons :white
               (shift-x
                101
                (shift-y
                 101
                 (circle-path 100 100))))
         (cons :red
               (shift-x
                101 (shift-y
                     101 (shift-path--
                          2 (circle-path 100 25)))))
         (cons :green
               (shift-x
                101 (shift-y
                     101 (shift-path-+
                          2 (circle-path 100 25)))))))
  (let ((p4 (mapcar #'(lambda (c) (c* 4 c)) (car tbox))))
    (paths/view:colored-multi-view
     (list (cons :white p4)
           (cons :green (inner-ticks 5 (shift-path-- 5 p4))))))
  (paths/view:view spiral-01 120 120)
  (paths/view:view
   (mapcar #'(lambda (c) (c* 100 c))
           (fill-inner-rectangle 0.1 '((0 . 0) (1 . 0) (1 . 1) (0 . 1))))
   110 110)
  (paths/view:view
   (shift-y 51
            (shift-x 51
                     (fill-inner-rectangle 5 (circle-path 50 120)))) 250 250))



