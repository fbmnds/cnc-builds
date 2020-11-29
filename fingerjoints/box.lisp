
(defpackage #:fingerjoints
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
           #:emitt))

(in-package #:fingerjoints)

(defun round_d-3 (x) (/ (round x 0.001) 1000))

(defun nflip-x (l) (mapc #'(lambda (c) (rplacd c (* -1 (cdr c)))) l))
;;(defun nflip-x (l) (dolist (c l) (rplacd c (* -1 (cdr c)))))

(defun flip-x (l) (mapcar #'(lambda (c) (cons (car c) (* -1 (cdr c)))) l))
(defun flip-y (l) (mapcar #'(lambda (c) (cons (* -1 (car c)) (cdr c))) l))
(defun flip-45 (l) (mapcar #'(lambda (c) (cons (cdr c) (car c))) l))

(defun shift-x (dx l) (mapcar #'(lambda (c) (cons (+ dx (car c)) (cdr c))) l))
(defun shift-y (dy l) (mapcar #'(lambda (c) (cons (car c) (+ dy (cdr c)))) l))
(defun shift-flip (dy l)
  (mapcar #'(lambda (c) (if (zerop (cdr c))
                            (cons (car c) dy)
                            (cons (car c) 0)))
          l))

(defun cut-bd (h l &optional (eps 0.001))
  (let ((l_ (remove-if-not #'(lambda (c) (< (car c) (- h eps))) l)))
    (append (list (cons h (cdar l_))) l_ (list (cons h (cdar (last l_)))))))

(defun cut-ac (h l &optional (eps 0.001))
  (let ((l_ (remove-if-not #'(lambda (c) (< (cdr c) (- h eps))) l)))
    (append (list (cons (caar l_) h)) l_ (list (cons (caar (last l_)) h)))))

(defun points-1 (n d1 d2 dy)
  (apply #'append
         (loop for i from 0 to (* 2 n)
               for i2 = (round_d-3 (/ i 2.))
               collect (if (evenp i)
                           (list (cons (* i2 (+ d1 d2)) 0)
                                 (cons (+ (* (1+ i2) d1) (* i2 d2)) 0))
                           (list (cons (+ (* (+ 0.5 i2) d1) (* (floor i2) d2)) dy)
                                 (cons (* (+ d1 d2) (+ 0.5 i2)) dy))))))

(defun points-var-1 (l d1 d2 dy)
  (let* ((d12 (+ d1 d2))
         (m (floor (/ l d12)))
         (tail (- l (* m d12)))
         (tail-d1 (- tail d1))
         (n (if (> tail-d1 0.) m (1- m)))
         (off (if (> tail-d1 0.)
                  (round_d-3 (/ tail-d1 2.))
                  (round_d-3 (/ (+ tail d1) 2.)))))
    (concatenate 'list
                 (list (cons 0 0))
                 (cdr (shift-x off (points-1 n d1 d2 dy)))
                 (list (cons l 0)))))

;; size lx * ly
(defun box-var-inner (lx ly d1-1 d1-2 d2-1 d2-2 dy)
  (let* ((l1 (- lx (* 2 dy)))
         (l2 (- ly (* 2 dy)))
         (p-l1 (points-var-1 l1 d1-1 d1-2 dy))
         (p-a (shift-flip dy p-l1))
         (p-c (reverse p-l1))
         (p-l2 (points-var-1 l2 d2-1 d2-2 dy))
         (p-b (flip-45 (shift-flip dy p-l2)))
         (p-d (flip-45 (reverse p-l2))))
    (concatenate 'list
                 (shift-x dy p-a)
                 (shift-y dy p-b)
                 (shift-x dy (shift-y (+ l2 dy) p-c))
                 (shift-x (+ l1 dy) (shift-y dy p-d)))))

;; size lx * ly
(defun box-var-outer-1 (lx ly d1-1 d1-2 d2-1 d2-2 dy)
  (let* ((l1 (- lx (* 2 dy)))
         (l2 (- ly (* 2 dy)))
         (p-a (points-var-1 l1 d1-1 d1-2 dy))
         (p-c (shift-x dy (reverse (shift-flip dy p-a))))
         (p-l2 (concatenate 'list
                            (list (cons 0 0))
                            (shift-x dy (points-var-1 l2 d2-1 d2-2 dy))
                            (list (cons ly 0))))
         (p-b (flip-45 (flip-x p-l2)))
         (p-d (flip-45 (reverse p-l2))))
    (concatenate 'list
                 (shift-x dy p-a)
                 (shift-x lx p-b)
                 (shift-y (+ l2 dy) p-c)
                 p-d)))

;; size lx * ly
(defun box-var-outer-2 (lx ly d1-1 d1-2 d2-1 d2-2 dy)
  (let* ((l1 (- lx (* 2 dy)))
         (l2 (- ly (* 2 dy)))
         (p-a (shift-x dy (points-var-1 l1 d1-1 d1-2 dy)))
         (p-c (reverse (shift-flip dy p-a)))
         (p-l2 (concatenate 'list
                            (list (cons 0 0))
                            (shift-x dy (points-var-1 l2 d2-1 d2-2 dy))
                            (list (cons ly 0))))
         (p-d (flip-45 (reverse (flip-x p-l2))))
         (p-b (flip-45 p-l2)))
    (concatenate 'list
                 p-a
                 (shift-x (+ l1 dy) p-b)
                 (shift-y (+ l2 dy) p-c)
                 (shift-x dy p-d))))

(defun eql-c (eps)
  (lambda (c1 c2)
    (and (< (round_d-3 (abs (- (car c1) (car c2)))) eps)
         (< (round_d-3 (abs (- (cdr c1) (cdr c2)))) eps))))

(defun eql-l (eps l1 l2)
  (reduce #'(lambda (x y) (and x y))
          (mapcar #'(lambda (c1 c2) (funcall (eql-c eps) c1 c2)) l1 l2)
          :initial-value t))

(defmacro eql_d-3 (l1 l2) `(eql-l 0.001 ,l1 ,l2))

(defun choose (c1 c2)
  (cond ((< (car c1) (car c2)) c1)
        ((> (car c1) (car c2)) c2)
        (t (if (< (cdr c1) (cdr c2)) c1 c2))))

(defun find-lower-left-corner (l)
  (unless (endp l)
    (reduce #'choose l :initial-value (car l))))

(defun shift-to-llc (l)
  (unless (endp l)
    (if (cdr l)
        (let ((pos (position (find-lower-left-corner l) l)))
          (if (zerop pos) l (append (subseq l pos) (subseq l 0 pos))))
        l)))

(defun box (lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-x spacer-y spacer-z)
  (let ((inner-box (box-var-inner lx ly d1-ac d2-ac d1-bd d2-bd dy))
        (outer-box-1 (box-var-outer-1 ly lz d1-ac d2-ac d1-bd d2-bd dy))
        (outer-box-2 (box-var-outer-2 lx lz d1-ac d2-ac d1-bd d2-bd dy)))
    (list
     inner-box
     (shift-x spacer-x inner-box)
     (shift-y spacer-z outer-box-2)
     (shift-x spacer-x (shift-y spacer-z outer-box-2))
     (shift-x (* 2 spacer-x) outer-box-1)
     (shift-x (+ (* 2 spacer-x) spacer-y) outer-box-1))))

(defun box-z (h lx ly lz d1-ac d2-ac d1-bd d2-bd dy spacer-x spacer-y spacer-z)
  (let ((inner-box (box-var-inner lx ly d1-ac d2-ac d1-bd d2-bd dy))
        (outer-box-1 (box-var-outer-1 ly lz d1-ac d2-ac d1-bd d2-bd dy))
        (outer-box-2 (box-var-outer-2 lx lz d1-ac d2-ac d1-bd d2-bd dy)))
    (list
     inner-box
     (shift-x spacer-x inner-box)
     (shift-y spacer-z (cut-ac h outer-box-2))
     (shift-x spacer-x (shift-y spacer-z (cut-ac h outer-box-2)))
     (shift-x (* 2 spacer-x) (cut-bd h outer-box-1))
     (shift-x (+ (* 2 spacer-x) spacer-y) (cut-bd h outer-box-1)))))

(defun emitt-c (c &optional (v 3))
  (if (endp c)
      ""
      (format nil "[~v$,~a]" v (car c) (format nil "~v$" v (cdr c)))))

(defun emitt (l &optional (v 3))
  (let ((l (mapcar #'(lambda (c) (emitt-c c v)) l)))
    (format nil "polygon([~a]);"
            (reduce #'(lambda (c1 c2)
                        (if c1
                            (format nil "~a,~a" c1 c2)
                            c2))
                    l :initial-value nil))))


(defun emitt-box (b &optional (v 3))
  (reduce #'(lambda (l1 l2)
              (if l1
                  (format nil "~a~%~a" l1 (emitt l2 v))
                  (emitt l2 v)))
          b :initial-value nil))

(defpackage #:fingerjoints/tests
  (:use #:cl #:fingerjoints)
  (:export #:run-tests))

(in-package #:fingerjoints/tests)

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
