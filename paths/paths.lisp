
(in-package #:paths)


(defparameter *precision* 0.000001)

;; On Lisp, pp. 47, 219, 410(?)

(defun group-3 (l)
  (let ((c0 (car l))
        (c1 (cadr l)))
    (labels ((rec (l acc)
               (cond ((and (car l) (cadr l) (caddr l))
                      (rec (cdr l)
                           (cons (list (car l) (cadr l) (caddr l)) acc)))
                     ((and (car l) (cadr l))
                      (rec (cdr l)
                           (cons (list (car l) (cadr l) c0) acc)))
                     ((car l)
                      (rec (cdr l)
                           (cons (list (car l) c0 c1) acc)))
                     (t (nreverse acc)))))
      (when l (rec l nil)))))

(defun group-2 (l)
  (let ((head (car l)))
    (labels ((rec (l acc)
               (cond ((and (car l) (cadr l))
                      (rec (cdr l)
                           (push (cons (car l) (cadr l)) acc)))
                     (t (nreverse (cons (cons (cdar acc) head) acc))))))
      (when l (rec l nil)))))

(defun round* (x)
  (cond ((numberp x) (* (round x *precision*) *precision*))
        ((and (consp x) (eql :tag (car x))) (round* (cdr x)))
        ((and (listp x) (consp (car x)) (consp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        ((and (consp x) (numberp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        ((and (consp x) (consp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        (t (error (format nil "round* undefined for ~a" x)))))

(defun zerop* (x) (when (> *precision* (abs x)) t))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun expand-call (type expr)
    `(,(car expr) ,@(mapcar #'(lambda (a)
                                `(with-type ,type ,a))
                            (cdr expr)))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun binarize (expr)
    (if (and (nthcdr 3 expr)
             (member (car expr) '(+ - * /)))
        (destructuring-bind (op a1 a2 . rest) expr
          (binarize `(,op (,op ,a1 ,a2) ,@rest)))
        expr)))

(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
                   expr
                   (expand-call type (binarize expr)))))

(defun poly (a b x)
  (with-type fixnum (+ (* a (expt x 2)) (* b x))))

;;;

(defun xy-p (c) (and (consp c) (numberp (car c)) (numberp (cdr c))))
(defun xyz-p (c) (and (consp c) (xy-p (car c)) (numberp (cdr c))))
(defun tag-xyz-p (c) (and (consp c) (xy-p (car c)) (numberp (cdr c))))

(deftype coord-xy () '(satisfies xy-p))
(deftype coord-xyz () '(satisfies xyz-p))
(deftype coord-tag-xyz () '(satisfies tag-xyz-p))
(deftype coord () '(or coord-xy coord-xyz))

;;;

(defun c-x (c)
  (typecase c
    (coord-xy (round* (car c)))
    (coord-xyz (c-x (car c)))
    (t (error (format nil "c-x undefined for ~a" c)))))

(defun c-y (c)
  (typecase c
    (coord-xy (round* (cdr c)))
    (coord-xyz (c-y (car c)))
    (t (error (format nil "c-y undefined for ~a" c)))))

(defun c-z (c)
  (typecase c
    (coord-xyz (round* (cdr c)))
    (t (error (format nil "c-z undefined for ~a" c)))))

(defun c- (c1 c2)
  (cond ((and (xy-p c1) (xy-p c2))
         (cons (- (c-x c1) (c-x c2)) (- (c-y c1) (c-y c2))))
        ((and (xyz-p c1) (xyz-p c2))
         (cons (cons (- (c-x c1) (c-x c2))
                     (- (c-y c1) (c-y c2)))
               (- (c-z c1) (c-z c2))))
        (t (error (format nil "c- undefined for ~a ~a" c1 c2)))))

(defun c2-c1- (c1-c2)
  (if (or (and (xy-p (car c1-c2)) (xy-p (cdr c1-c2)))
          (and (xyz-p (car c1-c2)) (xyz-p (cdr c1-c2))))
      (c- (cdr c1-c2) (car c1-c2))
      (error (format nil "c2-c1- undefined for ~a" c1-c2))))

(defun c+ (c1 c2)
  (cond ((and (xy-p c1) (xy-p c2))
         (cons (+ (c-x c1) (c-x c2)) (+ (c-y c1) (c-y c2))))
        ((and (xyz-p c1) (xyz-p c2))
         (cons (cons (+ (c-x c1) (c-x c2))
                     (+ (c-y c1) (c-y c2)))
               (+ (c-z c1) (c-z c2))))
        (t (error (format nil "c+ undefined for ~a ~a" c1 c2)))))

(defun c* (r c)
  (cond ((and (numberp r) (xy-p c))
         (cons (round* (* r (c-x c))) (round* (* r (c-y c)))))
        ((and (numberp r) (xyz-p c))
         (cons (c* r (car c)) (round* (* r (cdr c)))))
        (t (error (format nil "c* undefined for ~a ~a" r c)))))

(defun euklid (c)
  (typecase c
    (coord-xy (round* (sqrt (+ (* (c-x c) (c-x c))
                               (* (c-y c) (c-y c))))))
    (coord-xyz (round* (sqrt (+ (* (c-x c) (c-x c))
                                (* (c-y c) (c-y c))
                                (* (c-z c) (c-z c))))))
    (t (error (format nil "euklid undefined for ~a" c)))))

(defun c= (c1 c2)
  (cond ((and (numberp c1) (numberp c2))
         (> *precision* (- c1 c2)))
        ((and (typep c1 'coord) (typep c2 'coord))
         (> *precision* (euklid (c- c1 c2))))
        ((and (listp c1) (listp c2))
         (and (mapcar #'c= c1 c2)))
        (t (error (format nil "c= undefined for ~a ~a" c1 c2)))))

(defun c1-c2= (s1 s2) (and (c= (car s1) (car s2)) (c= (cdr s1) (cdr s2))))

(defun c-normed (c)
  (typecase c
    (coord-xy (c* (/ 1. (euklid c)) c))
    (t (error (format nil "c-normed undefined for ~a" c)))))

(defun normale-+ (c1 c2)
  (let ((d (c-normed (c- c1 c2))))
    (cond ((c= c1 c2) (error "undefined normale on zero vector"))
          (t (round* (cons (* -1.0 (c-y d)) (c-x d)))))))

(defun normale-- (c1 c2) (c* -1.0 (normale-+ c1 c2)))

(defun det2 (c1 c2) (round* (- (* (c-x c1) (c-y c2)) (* (c-x c2) (c-y c1)))))

(defun collinear-2d (c0 c1 c2 &optional (eps 0.00001))
  "Return the segment (C0 . C2), iff the points C0, C1 and C2 are collinear."
  (let ((x1 (car c0))
        (y1 (cdr c0))
        (x2 (car c1))
        (y2 (cdr c1))
        (x3 (car c2))
        (y3 (cdr c2)))
    (when (> eps (abs (+ (* x1 (- y2 y3))
                         (* x2 (- y3 y1))
                         (* x3 (- y1 y2)))))
      (cons c0 c2))))

;; M = a b
;;     c d
;; det2 M = ad - cb
;; M^-1 = 1/det2 * d -b
;;                 -c a

(defun linear-solution (m1 m2 b)
  (ignore-errors
   (let ((det (det2 m1 m2)))
     (round* (cons (/ (+ (* (cdr m2) (car b)) (* (cdr m1) (cdr b))) det)
                   (/ (- (* (car m1) (cdr b)) (* (car m2) (car b))) det))))))

(defun radius-center-in-corner (c1 c2 c3 c4)
  (ignore-errors
   (round* (c+ c2
               (c* (car (c+ c2 (c* (car (linear-solution (normale-+ c1 c2)
                                                         (normale-+ c3 c4)
                                                         (c- c3 c2)))
                                   (normale-+ c1 c2))))
                   (normale-+ c1 c2))))))

#|
(paths:radius-center-in-corner '(0 . 0) '(0 . 1) '(0.5 . 1.5) '(1.5 . 1.5))
=> '(0.5 . 1)
(paths:radius-center-in-corner '(0.5 . 1.5) '(1.5 . 1.5) '(2 . 1) '(2 . 0))
!=> '(1.5 . 1)
(paths:radius-center-in-corner '(2 . 1) '(2 . 0) '(1.5 . -0.5) '(1 . -0.5))
!=> '(1.5 . 0)
(paths:radius-center-in-corner '(1.5 . -0.5) '(0.5 . -0.5) '(0 . 0) '(0 . 1))
=> (0.5 . 0)
|#

(defun shift-corner-+ (r c1 c2 c3)
  (let* ((n1 (normale-+ c2 c1))
         (n2 (normale-+ c3 c2))
         (c-1-2 (c- c1 c2))
         (det (det2 c-1-2 (c- c3 c2)))
         (l (/ (* r (+ (* (- (cdr c3) (cdr c2)) (- (car n1) (car n2)))
                       (* (- (car c2) (car c3)) (- (cdr n1) (cdr n2)))))
               det)))
    (c+ (c* r n1) (c- c2 (c* l c-1-2)))))

(defun shift-corner-- (r c1 c2 c3)
  (let* ((n1 (normale-- c2 c1))
         (n2 (normale-- c3 c2))
         (c-1-2 (c- c1 c2))
         (det (det2 c-1-2 (c- c3 c2)))
         (l (/ (* r (+ (* (- (cdr c3) (cdr c2)) (- (car n1) (car n2)))
                       (* (- (car c2) (car c3)) (- (cdr n1) (cdr n2)))))
               det)))
    (c+ (c* r n1) (c- c2 (c* l c-1-2)))))

(defun in-row-p (c)
  (ignore-errors (or (= (caar c) (caadr c) (caaddr c))
                     (= (cdar c) (cdadr c) (cdaddr c)))))

(defun stats (p)
  (loop for c in p
        counting c into len
        maximizing (car c) into max-x
        minimizing (car c) into min-x
        maximizing (cdr c) into max-y
        minimizing (cdr c) into min-y
        finally
        (return (list :len len :max-x max-x :min-x min-x
                      :max-y max-y :min-y min-y))))

(defun stats-acc (ps)
  (loop for s in (mapcar #'stats ps)
        summing (nth 1 s) into len
        maximizing (nth 3 s) into max-x
        minimizing (nth 5 s) into min-x
        maximizing (nth 7 s) into max-y
        minimizing (nth 9 s) into min-y
        finally
        (return (list :len len :max-x max-x :min-x min-x
                      :max-y max-y :min-y min-y))))

(defun trim-path (p &optional (len (length p)))
  (labels ((not-dup (j)
             (cond ((zerop j))
                   (t (not (c= (nth (1- j) p) (nth j p))))))
           (test (len-1 q j)
             (cond ((zerop j))
                   ((= j len-1) (not (c= (nth 0 q) (nth j q))))
                   (t (not (in-row-p (list (nth (1- j) q)
                                           (nth j q)
                                           (nth (1+ j) q))))))))
    (let* ((q (loop for i from 0 to (1- len)
                    when (not-dup i) collect (nth i p)))
           (len-1 (1- (length q))))
      (loop for i from 0 to len-1 when (test len-1 q i) collect (nth i q)))))

(defun shift-path-+ (r l)
  (remove-if #'null
             (mapcar #'(lambda (c)
                         (ignore-errors
                          (shift-corner-+ r (car c) (cadr c) (caddr c))))
                     (group-3 (trim-path l)))))

(defun shift-path-- (r l)
  (remove-if #'null
             (mapcar #'(lambda (c)
                         (ignore-errors
                          (shift-corner-- r (car c) (cadr c) (caddr c))))
                     (group-3 (trim-path l)))))

(defun circle-path (r n)
  (let ((pi2/n (/ (* 2 pi) n)))
    (loop for i from 0 to (1- n) collect (cons (* r (cos (* i pi2/n)))
                                               (* r (sin (* i pi2/n)))))))

(defun min-xy-path (path)
  (reduce
   #'(lambda (c d) (cons (min (car c) (car d)) (min (cdr c) (cdr d))))
   (cdr path) :initial-value (cons (caar path) (cdar path))))

(defun flip-path (path) (mapcar #'(lambda (c) (cons (cdr c) (car c))) path))

(defun shift-path-origin (path &optional (offset (cons 0. 0.)))
  (let ((delta (min-xy-path path)))
    (mapcar #'(lambda (c) (cons (+ offset (- (car c) (car delta)))
                                (+ offset (- (cdr c) (cdr delta)))))
            path)))

#|
(defparameter box (car fingerjoints/tests::e-box))                    ;

(setf box (group 3 (mapcar #'(lambda(c) (cons (coerce (car c) 'double-float) 
                                              (coerce (cdr c) 'double-float))) 
                           box)))

(mapcar #'(lambda (c)                                              ;
            (ignore-errors (shift-corner-+ 1.5d0 (car c) (cadr c) (caddr c)))) ;
        box)                                    ;

(remove-if #'null *)
(fingerjoints:emitt *)

(with-open-file (s "/home/dev/Desktop/box.scad" :direction :output :if-exists :supersede) (format s "~a" *))
                                      ;
|#

(defun deg->rad (x) (* x (/ pi 180)))

(defun rad->deg (x) (* x (/ 180 pi)))

(defun polar->cartesian (c)
  (destructuring-bind (r . alfa) c (c* r (cons (cos alfa) (sin alfa)))))

(defun cartesian->polar (c &optional (eps 0.00001))
  (destructuring-bind (x . y) c
    (let ((r (sqrt (+ (* x x) (* y y)))))
      (if (> eps r)
          (cons 0 0)
          (cons r (mod (asin (/ y r)) (* 2 pi)))))))

(defun radian (s0-polar s1-polar &optional (eps 0.00001))
  (assert (> eps 0))
  (destructuring-bind (r0 . alfa0) s0-polar
    (destructuring-bind (r1 . alfa1) s1-polar
      (let ((r-avg (/ (+ r1 r0) 2))
            (dalfa (mod (- alfa1 alfa0) (* 2 pi))))
        (cond ((> dalfa eps) (* dalfa r-avg))
              ((< dalfa (* -1 eps)) (* dalfa r-avg))
              (t (* 2 pi r-avg)))))))

(defun n-d-polar (dxy s0-polar s1-polar &optional (eps 0.00001))
  (destructuring-bind (r0 . alfa0) s0-polar
    (destructuring-bind (r1 . alfa1) s1-polar
      (let ((n (round (/ (radian s0-polar s1-polar eps) dxy)))
            (dalfa (mod (- alfa1 alfa0) (* 2 pi))))
        (if (> eps dalfa)
            (cons n (cons (/ (- r1 r0) n) (/ (* 2 pi) n)))
            (cons n (cons (/ (- r1 r0) n) (/ dalfa n))))))))

(defun spiral-polar (dxy s0-polar s1-polar &optional (eps 0.00001))
  (destructuring-bind (r0 . alfa0) s0-polar
    (destructuring-bind (n . (dr . dalfa)) (n-d-polar dxy s0-polar s1-polar eps)
      (let ((spiral '()))
        (dotimes (i n)
          (push (cons (+ (* i dr) r0) (+ (* i dalfa) alfa0)) spiral))
        (push s1-polar spiral)
        (nreverse spiral)))))

(defun spiral (dxy s0 s1 &optional (eps 0.00001))
  (let ((s0-polar ( cartesian->polar s0))
        (s1-polar ( cartesian->polar s1)))
    (mapcar #'polar->cartesian
            (spiral-polar dxy s0-polar s1-polar eps))))


