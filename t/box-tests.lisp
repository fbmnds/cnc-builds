
(in-package #:paths/box-tests)

(defparameter l1 '((1 . 1) (11 . 1) (11 . 5) (1 . 5)))
(defparameter l2 '((0 . 0) (10 . 0) (10 . 5) (15 . 5)))

(defparameter d1-ac 4)
(defparameter d2-ac 4)
(defparameter d1-bd 4)
(defparameter d2-bd 4)
(defparameter dy  2.5)
(defparameter eps 0.01)

(defparameter lx (+ 50 (* 2 dy)))
(defparameter ly (+ 20 (* 2 dy)))
(defparameter lz ly)
(defparameter spacer-x (+ lx 4))
(defparameter spacer-y (+ ly 4))
(defparameter spacer-z (+ lz 4))

(defparameter tbox
  (box lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-x spacer-y spacer-z))

(defparameter e-box (emitt-box tbox))

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
                 '(:LEN 58 :MAX-X 55.0 :MIN-X 0 :MAX-Y 25.0 :MIN-Y 0)))
  (assert (equal (stats-acc tbox)
                 (:LEN 308 :MAX-X 172.0 :MIN-X 0 :MAX-Y 54.0 :MIN-Y 0)))
  
  (let ((x '(1 2 3 4 5)))
    (assert (equal (group 3 x)
                   '((1 2 3) (2 3 4) (3 4 5) (4 5 1) (5 1 2))))
    (setf x '(1 2 3 4 5 6))
    (assert (equal (group 3 x)
                   '((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 1) (6 1 2))))
    (setf x '((2.5 . 2.5) (9.5 . 2.5) (9.5 . 0)))
    (assert (equal (shift-corner-+ 1.5 (car x) (cadr x) (caddr x))
                   '(11.0 . 4.0)))))

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
   (list (cons :white rhomb)
         (cons :red (shift-path-- 1.5 rhomb))
         (cons :green (shift-path-+ 1.5 rhomb))))
  (paths/view:colored-multi-view
   (list (cons :white
               (paths/box:shift-x 101
                                  (paths/box:shift-y 101
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



