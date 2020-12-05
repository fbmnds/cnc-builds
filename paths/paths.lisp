
(in-package #:paths)

;; On Lisp, pp. 47, 219, 410

(defun group (n source)
  (if (zerop n) (error "group on zero length list"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (cond ((consp rest)
                      (let ((sub (subseq source 0 n)))
                        (if (= n (length sub))
                            (rec (cdr source) (cons sub acc)))))
                     ((= n (length source)) (nreverse (cons source acc)))
                     (t (nreverse acc))))))
    (if source (rec source nil) nil)))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a)
                              `(with-type ,type ,a))
                          (cdr expr))))

(defun binarize (expr)
  (if (and (nthcdr 3 expr)
           (member (car expr) '(+ - * /)))
      (destructuring-bind (op a1 a2 . rest) expr
        (binarize `(,op (,op ,a1 ,a2) ,@rest)))
      expr))

(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
                   expr
                   (expand-call type (binarize expr)))))

(defun poly (a b x)
  (with-type fixnum (+ (* a (expt x 2)) (* b x))))

;;;

(defun df-car (c)
  (declare (type (cons double-float double-float) c))
  (the double-float (car c)))

(defun df-cdr (c)
  (declare (type (cons double-float double-float) c))
  (the double-float (cdr c)))

(defun c- (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (cons (- (df-car c1) (df-car c2)) (- (df-cdr c1) (df-cdr c2))))

(defun c+ (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (cons (+ (df-car c1) (df-car c2)) (+ (df-cdr c1) (df-cdr c2))))

(defun c* (r c)
  (declare (type (cons double-float double-float) c)
           (type double-float r))
  (cons (the double-float (* r (df-car c)))
        (the double-float (* r (df-cdr c)))))

(defun euklid (c)
  (declare (type (cons double-float double-float) c))
  (let ((ca (df-car c))
        (cd (df-cdr c)))
    (with-type double-float (sqrt (+ (expt ca 2.d0)
                                     (expt cd 2.d0))))))

(defun c= (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (and (= (df-car c1) (df-car c2)) (= (df-cdr c1) (df-cdr c2))))

(defun c-normed (c)
  (declare (type (cons double-float double-float) c))
  (let ((e2 (euklid c)))
    (cons (the double-float (/ (df-car c) e2))
          (the double-float (/ (df-cdr c) e2)))))

(defun normale-+ (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (let* ((d (c-normed (c- c1 c2)))
         (da (df-car d))
         (dd (df-cdr d)))
    (cond ((c= c1 c2) (error "undefined normale on zero vector"))
          (t (cons (* -1.d0 dd) da)))))

(defun normale-- (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (c* -1.d0 (normale-+ c1 c2)))

(defun det2 (c1 c2)
  (declare (type (cons double-float double-float) c1 c2))
  (let ((a (df-car c1))
        (c (df-cdr c1))
        (b (df-car c2))
        (d (df-cdr c2)))
    (with-type double-float (- (* a d) (* b c)))))

;; M = a b
;;     c d
;; det M = ad - cb
;; M-1 = 1/det * d -b
;;               -c a

(defun shift-corner-+ (r c1 c2 c3)
  (declare (type (cons double-float double-float) c1 c2 c3)
           (type double-float r))
  (let* ((n1 (normale-+ c2 c1))
         (n2 (normale-+ c3 c2))
         (n11 (df-car n1))
         (n12 (df-cdr n1))
         (n21 (df-car n2))
         (n22 (df-cdr n2))
         ;;(c11 (df-car c1))
         ;;(c12 (df-cdr c1))
         (c21 (df-car c2))
         (c22 (df-cdr c2))
         (c31 (df-car c3))
         (c32 (df-cdr c3))
         (det (det2 (c- c1 c2) (c- c3 c2)))
         (l (with-type double-float
              (/ (* r (+ (* (- c32 c22) (- n11 n21))
                         (* (- c21 c31) (- n12 n22))))
                 det))))
    (c+ (c* r n1) (c+ (c* l (c- c2 c1)) c2))))

(defun shift-corner-- (r c1 c2 c3)
  (declare (type (cons double-float double-float) c1 c2 c3)
           (type double-float r))
  (let* ((n1 (normale-- c2 c1))
         (n2 (normale-- c3 c2))
         (n11 (df-car n1))
         (n12 (df-cdr n1))
         (n21 (df-car n2))
         (n22 (df-cdr n2))
         ;;(c11 (df-car c1))
         ;;(c12 (df-cdr c1))
         (c21 (df-car c2))
         (c22 (df-cdr c2))
         (c31 (df-car c3))
         (c32 (df-cdr c3))
         (det (det2 (c- c1 c2) (c- c3 c2)))
         (l (with-type double-float
              (/ (* r (+ (* (- c32 c22) (- n11 n21))
                         (* (- c21 c31) (- n12 n22))))
                 det))))
    (c+ (c* r n1) (c+ (c* l (c- c2 c1)) c2))))

(defun shift-path-+ (r l)
  (declare (type double-float r))
  (mapcar #'(lambda (c)
              (shift-corner-+ r (car c) (cadr c) (caddr c)))
          (group 3 l)))

(defun shift-path-- (r l)
  (declare (type double-float r))
  (mapcar #'(lambda (c)
              (shift-corner-- r (car c) (cadr c) (caddr c)))
          (group 3 l)))


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

