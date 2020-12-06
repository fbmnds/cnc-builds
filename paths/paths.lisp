
(in-package #:paths)

;; On Lisp, pp. 47, 219, 410

(defun group (n l)
  (when (zerop n) (error "cannot group with 0"))
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

(defun shift-path-+ (r l)
  (mapcar #'(lambda (c)
              (ignore-errors (shift-corner-+ r (car c) (cadr c) (caddr c))))
          (group 3 l)))

(defun shift-path-- (r l)
  (mapcar #'(lambda (c)
              (ignore-errors (shift-corner-- r (car c) (cadr c) (caddr c))))
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

