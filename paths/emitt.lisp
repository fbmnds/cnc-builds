
(in-package #:paths/emitt)


(defun group-2 (l)
  (let ((head (car l)))
    (labels ((rec (l acc)
               (cond ((and (car l) (cadr l))
                      (rec (cdr l)
                           (push (cons (car l) (cadr l)) acc)))
                     (t (nreverse (cons (cons (cdar acc) head) acc))))))
      (when l (rec l nil)))))

(defun emitt-scad-cons (c &optional (v 3))
  (if (endp c)
      ""
      (format nil "[~v$,~a]" v (car c) (format nil "~v$" v (cdr c)))))

(defun emitt-scad (l &optional (v 3))
  (let ((l (mapcar #'(lambda (c) (emitt-scad-cons c v)) l)))
    (format nil "polygon([~a]);"
            (reduce #'(lambda (c1 c2)
                        (if c1
                            (format nil "~a,~a" c1 c2)
                            c2))
                    l :initial-value nil))))

(defun emitt-scad-box (b &optional (v 3))
  (reduce #'(lambda (l1 l2)
              (if l1
                  (format nil "~a~%~a" l1 (emitt-scad l2 v))
                  (emitt-scad l2 v)))
          b :initial-value nil))

(defun emitt-gcode-path (path &optional (v 3) (eps 0.001))
  (remove-if #'null
             (mapcar #'(lambda (l)
                         (let ((c-dx (- (cadr l) (caar l)))
                               (c-dy (- (cddr l) (cdar l))))
                           (cond ((and (< eps (abs c-dx)) (< eps (abs c-dy)))
                                  (format nil "G01 X~v$Y~v$" v c-dx v c-dy))
                                 ((< eps (abs c-dx))
                                  (format nil "G01 X~v$" v c-dx))
                                 ((< eps (abs c-dy))
                                  (format nil "G01 Y~v$" v c-dy))
                                 (t nil))))
                     (group-2 path))))

(defun insert-tag (c1-c2 w/2)
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (mid (c* 0.5 (c- c2 c1)))
         (mid-w/2 (c* w/2 (c-normed (c- mid c1))))
         (w- (c- mid mid-w/2))
         (w+ (c+ mid mid-w/2)))
    (list (cons c1 w-) (cons :tag (cons w- w+)) (cons w+ c2))))

(defun divide-segment (c1-c2 n)
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (v (c- c2 c1))
         (ret nil))
    (dotimes (i n) (push (c+ c1 (c* (/ i n) v)) ret))
    (push c2 ret)
    (nreverse ret)))

(defun inner-divide-segment (c1-c2 n)
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (v (c- c2 c1))
         (ret nil))
    (dotimes (i n) (unless (zerop i) (push (c+ c1 (c* (/ i n) v)) ret)))
    (nreverse ret)))

#|
PATHS/EMITT> (insert-tag (cons (cons 0 0) (cons 2 3)) 0.4)
((1.0 . 1.5) (0.22188008 . 0.33282015) (0.7781199 . 1.1671798)
 (1.2218801 . 1.8328202))
PATHS/EMITT> (euklid (c- '(1.2218801 . 1.8328202) '(0.7781199 . 1.1671798)))
0.80000013 (80.000015%)
|#

(defun emitt-gcode (path f dz nz &optional (fz f) (v 3) (eps 0.001))
  (let* ((gc-path (emitt-gcode-path path v eps))
         (gc-z (format nil "G01 Z~v$ F~v$" v dz v fz))
         (gc-f (format nil "~a F~v$" (car gc-path) v f))
         (gcode nil))
    (dotimes (i nz) (push (append (list gc-z gc-f) (cdr gc-path)) gcode))
    (apply #'append gcode)))

 
