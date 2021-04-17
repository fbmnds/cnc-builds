
(in-package #:paths/box-tests)

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
                 '(:LEN 90 :MAX-X 65.0 :MIN-X 0 :MAX-Y 45.0 :MIN-Y 0)))
  (assert (equal (stats-acc tbox)
                 '(:LEN 436 :MAX-X 192.0 :MIN-X 0 :MAX-Y 74.0 :MIN-Y 0)))
  
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
                   (paths/emitt::convert-path-dxyz%
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
                    -0.5 1200)))))

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
               (paths/box:shift-x
                101
                (paths/box:shift-y
                 101
                 (paths:circle-path 100 100))))
         (cons :red
               (paths/box:shift-x
                101 (paths/box:shift-y
                     101 (paths:shift-path--
                          2 (paths:circle-path 100 25)))))
         (cons :green
               (paths/box:shift-x
                101 (paths/box:shift-y
                     101 (paths:shift-path-+
                          2 (paths:circle-path 100 25))))))))



