
(in-package #:paths)

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

(defun c- (c1 c2) (cons (- (car c1) (car c2)) (- (cdr c1) (cdr c2))))

(defun c+ (c1 c2) (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

(defun c* (r c) (cons (* r (car c)) (* r (cdr c))))

(defun euklid (c) (sqrt (+ (expt (car c) 2) (expt (cdr c) 2))))

(defun c= (c1 c2) (and (= (car c1) (car c2)) (= (cdr c1) (cdr c2))))

(defun c-normed (c)
  (let ((e2 (euklid c)))
    (cons (/ (car c) e2) (/ (cdr c) e2))))

(defun normale-+ (c1 c2)
  (let ((d (c-normed (c- c1 c2))))
    (cond ((c= c1 c2) (error "undefined normale on zero vector"))
          (t (cons (* -1.0 (cdr d)) (car d))))))

(defun normale-- (c1 c2) (c* -1.0 (normale-+ c1 c2)))

(defun det2 (c1 c2) (- (* (car c1) (cdr c2)) (* (car c2) (cdr c1))))

;; M = a b
;;     c d
;; det2 M = ad - cb
;; M^-1 = 1/det2 * d -b
;;                 -c a

(defun linear-solution (m1 m2 b)
  (ignore-errors
   (let ((det (det2 m1 m2)))
     (cons (/ (+ (* (cdr m2) (car b)) (* (cdr m1) (cdr b))) det)
           (/ (- (* (car m1) (cdr b)) (* (car m2) (car b))) det)))))

(defun radius-center-in-corner (c1 c2 c3 c4)
  (ignore-errors
   (c+ c2 (c*
           (car (c+ c2
                    (c* (car (linear-solution (normale-+ c1 c2)
                                              (normale-+ c3 c4)
                                              (c- c3 c2)))
                        (normale-+ c1 c2))))
           (normale-+ c1 c2)))))

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

