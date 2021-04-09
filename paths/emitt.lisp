
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
         (mid (c+ c1 (c* 0.5 (c- c2 c1))))
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

(defun c1-c2= (s1 s2) (and (c= (car s1) (car s2)) (c= (cdr s1) (cdr s2))))

(defun insert-tags (path tags w/2)
  (let ((p2 (group-2 path)))
    (flet ((tag-it? (c1-c2)
             (if (member c1-c2 tags :test #'c1-c2=)
                 (insert-tag c1-c2 w/2)
                 c1-c2)))
      (mapcar #'tag-it? p2))))

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

(defun emitt-gcode-tagged-path
    (g2-path i nz &optional (nt (/ nz 2)) (v 3) (eps 0.001))
  (remove-if #'null
             (mapcar #'(lambda (c1-c2)
                         (if (eql :tag (car (c1-c2)))
                             (let ((c1-c2 (cdr c1-c2)))
                               nil)
                             (let ((c-dx (- (cadr c1-c2) (caar c1-c2)))
                                   (c-dy (- (cddr c1-c2) (cdar c1-c2))))
                               (cond ((and (< eps (abs c-dx))
                                           (< eps (abs c-dy)))
                                      (format nil "G01 X~v$Y~v$" v c-dx v c-dy))
                                     ((< eps (abs c-dx))
                                      (format nil "G01 X~v$" v c-dx))
                                     ((< eps (abs c-dy))
                                      (format nil "G01 Y~v$" v c-dy))
                                     (t nil)))))
                     g2-path)))


(defun c2-c1- (c1-c2) (c- (cdr c1-c2) (car c1-c2)))

#|
 (mapcar #'add-z g2-path)

add-z:
- untagged: (cons (c2-c1- c1-c2) (* i dz))
- tagged:  (if (< i (- nz nt))
              (cons (c2-c1- c1-c2) (* i dz))
              (list 
                ;; 
                (cons (c2-c1- c1-c2) 0.) ; -w/2
                ...)
|#

(defun emitt-gcode-grouped-path
    (g2-path f dz nz &optional (nt (/ nz 2)) (fz f) (v 3) (eps 0.001))
  (let* ((gc-path (emitt-gcode-path path v eps))
         (gc-z (format nil "G01 Z~v$ F~v$" v dz v fz))
         (gc-f (format nil "~a F~v$" (car gc-path) v f))
         (gcode nil))
    (dotimes (i nz) (push (append (list gc-z gc-f) (cdr gc-path)) gcode))
    (apply #'append gcode)))
 
