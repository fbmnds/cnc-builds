
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

(defparameter e-box
  (emitt-box
   (box lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-x spacer-y spacer-z)))

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

  (assert (eql_d-3 l2 (shift-to-llc '((1 . 5) (1 . 1) (11 . 1) (11 . 5))))))
