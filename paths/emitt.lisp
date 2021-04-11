
(in-package #:paths/emitt)

(defparameter *precision* 0.00001)

(defun round* (x)
  (cond ((numberp x) (* (round x *precision*) *precision*))
        ((and (consp x) (eql :tag (car x))) (round* (cdr x)))
        ((and (consp x) (numberp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        ((and (consp x) (consp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        (t (format nil "round* undefined for ~a" x))))

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

(defun emitt-gcode (path f dz nz &optional (fz f) (v 3) (eps 0.001))
  (let* ((gc-path (emitt-gcode-path path v eps))
         (gc-z (format nil "G01 Z~v$ F~v$" v dz v fz))
         (gc-f (format nil "~a F~v$" (car gc-path) v f))
         (gcode nil))
    (dotimes (i nz) (push (append (list gc-z gc-f) (cdr gc-path)) gcode))
    (apply #'append gcode)))

(defun insert-tag (c1-c2 w/2)
  "Insert a tagged centered path segment of length W between C1 and C2."
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (mid (c* 0.5 (c+ c2 c1)))
         (mid-w/2 (c* w/2 (c-normed (c- c2 c1))))
         (w- (c- mid mid-w/2))
         (w+ (c+ mid mid-w/2)))
    (list (cons c1 w-) (cons :tag (cons w- w+)) (cons w+ c2))))

(defun divide-segment (c1-c2 n)
  "Divide the path segment C1-C2 into N+1 equidistant coordinates C1 Cx... C2."
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (v (c- c2 c1))
         (ret nil))
    (dotimes (i n) (push (c+ c1 (c* (/ i n) v)) ret))
    (push c2 ret)
    (nreverse ret)))

(defun inner-divide-segment (c1-c2 n)
  "Return the LIST of the  N-1 inner equidistant coordinates between C1 and C2."
  (let* ((c1 (car c1-c2))
         (c2 (cdr c1-c2))
         (v (c- c2 c1))
         (ret nil))
    (dotimes (i n) (unless (zerop i) (push (c+ c1 (c* (/ i n) v)) ret)))
    (nreverse ret)))

(defun c1-c2= (s1 s2) (and (c= (car s1) (car s2)) (c= (cdr s1) (cdr s2))))

(defun insert-tags (path tags w/2)
  "Convert PATH into a LIST of tagged and untagged path segments."
  (let ((p2 (group-2 path))
        (ret nil))
    (flet ((push-it (c1-c2)
             (if (member c1-c2 tags :test #'c1-c2=)
                 (dolist (tag (insert-tag c1-c2 w/2)) (push tag ret))
                 (push c1-c2 ret))))
      (dolist (c1-c2 p2) (push-it c1-c2))
      (nreverse ret))))

(defun convert-dxyz (c1-c2 i dz nz &optional (nt (/ nz 2)) (eps 0.001))
  "Convert a tagged path segment into relative distances ((DX . DY) . DZ)."
  (let* ((tag-p (eql :tag (car c1-c2)))
         (c-dz (round* (* i dz)))
         (c1-c2 (if tag-p (cdr c1-c2) c1-c2))
         (c-dx (round* (- (cadr c1-c2) (caar c1-c2))))
         (c-dy (round* (- (cddr c1-c2) (cdar c1-c2)))))
    (if (and tag-p (> i (- nz nt)))
        (list (cons (cons 0. 0.) (* -1. c-dz))
              (cons (cons c-dx c-dy) 0.)
              (cons (cons 0. 0.) c-dz))
        (list (cons (cons c-dx c-dy) c-dz)))))

(defun optimize-relative-distances (path-dxyz &optional (eps 0.001))
  "Accumulate marginal and unidirectional movements."
  (let ((c-d0 (cons (cons (round* (caaar path-dxyz))
                          (round* (cdaar path-dxyz)))
                    (round* (cdar path-dxyz))))
        (c-d* (cons (cons 0. 0.) 0.))
        (ret nil))
    (dolist (c-dxyz (cdr path-dxyz))
      (let* ((c-dx (round* (caar c-dxyz)))
             (c-dy (round* (cdar c-dxyz)))
             (c-dz (round* (cdr c-dxyz)))
             (c-dx* (caar c-d*))
             (c-dy* (cdar c-d*))
             (c-dz* (cdr c-d*))
             (c-dxx* (round* (+ c-dx c-dx*)))
             (c-dyy* (round* (+ c-dy c-dy*)))
             (c-dzz* (round* (+ c-dz c-dz*))))
        (cond ((and (> (abs c-dxx*) eps)
                    (> (abs c-dyy*) eps)
                    (> (abs c-dyy*) eps))
               (setf c-d* (cons (cons 0. 0.) 0.))
               (push (cons (cons c-dxx* c-dyy*) c-dzz*) ret))
              ((and (> (abs c-dxx*) eps) (> (abs c-dyy*) eps))
               (setf c-d* (cons (cons 0. 0.) c-dzz*))
               (push (cons (cons c-dxx* c-dyy*) 0.) ret))
              ((and (> (abs c-dxx*) eps) (> (abs c-dzz*) eps))
               (setf c-d* (cons (cons 0. c-dyy*) 0.))
               (push (cons (cons c-dxx* 0.) c-dzz*) ret))
              ((and (> (abs c-dyy*) eps) (> (abs c-dzz*) eps))
               (setf c-d* (cons (cons c-dxx* 0.) 0.))
               (push (cons (cons 0. c-dyy*) c-dzz*) ret))
              ((> (abs c-dxx*) eps)
               (if (and (zerop (cdar c-d0)) (zerop (cdr c-d0)))
                   (setf c-d0
                         (cons (cons (+ (caar c-d0) c-dxx*) c-dyy*) c-dzz*))
                   (progn
                     (push c-d0 ret)
                     (setf c-d0 (cons (cons c-dxx* 0.) 0.))
                     (setf c-d* (cons (cons 0. c-dyy*) c-dzz*)))))
              ((> (abs c-dyy*) eps)
               (if (and (zerop (caar c-d0)) (zerop (cdr c-d0)))
                   (setf c-d0
                         (cons (cons c-dxx* (+ (cdar c-d0) c-dyy*)) c-dzz*))
                   (progn
                     (push c-d0 ret)
                     (setf c-d0 (cons (cons 0. c-dyy*) 0.))
                     (setf c-d* (cons (cons c-dxx* 0.) c-dzz*)))))
              ((> (abs c-dzz*) eps)
               (if (and (zerop (caar c-d0)) (zerop (cdar c-d0)))
                   (setf c-d0
                         (cons (cons c-dxx* c-dyy*) (+ (cdr c-d0) c-dzz*)))
                   (progn
                     (push c-d0 ret)
                     (setf c-d0 (cons (cons 0. 0.) c-dzz*))
                     (setf c-d* (cons (cons c-dxx* 0.) c-dzz*)))))
              (t
               (setf c-d* (cons (cons c-dxx* c-dyy*) c-dzz*))))))
    (push c-d0 ret) ;; might be marginal move in X, Y, Z
    (nreverse ret)))

(defun convert-path-dxyz (path tags w/2 dz nz
                          &optional (nt (/ nz 2)) (eps 0.001))
  "Convert a PATH with TAGS into optimized relative distances ((DX . DY) . DZ)."
  (let ((p2 (insert-tags path tags w/2))
        (ret nil))
    (dolist (i nz)
      (dolist (c1-c2 p2)
        (dolist (dxyz-c1-c2 (convert-dxyz c1-c2 i dz nz nt eps))
          (dolist (dxyz dxyz-c1-c2)
            (push dxyz ret)))))
    (nreverse ret)))

(defun convert-path-dxyz-2 (path tags w/2 dz nz
                          &optional (nt (/ nz 2)) (eps 0.001))
  "Convert a PATH with TAGS into optimized relative distances ((DX . DY) . DZ)."
  (let ((p2 (insert-tags path tags w/2))
        (ret nil))
    (dolist (i nz)
      (dolist (c1-c2 p2)
        (dolist (dxyz-c1-c2 (convert-dxyz c1-c2 i dz nz nt eps))
          (dolist (dxyz dxyz-c1-c2)
            (push dxyz ret)))))
    (optimize-relative-distances (nreverse ret))))

#|
(defun emitt-gcode-tagged-path
    (path tags nz &optional (nt (/ nz 2)) (v 3) (eps 0.001))
  (remove-if #'null
             (mapcar #'(lambda (c1-c2)
                         (if (eql :tag (car c1-c2))
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
                     path)))
|#

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


(defun emitt-gcode-grouped-path
    (g2-path f dz nz &optional (nt (/ nz 2)) (fz f) (v 3) (eps 0.001))
  (let* ((gc-path (emitt-gcode-path path v eps))
         (gc-z (format nil "G01 Z~v$ F~v$" v dz v fz))
         (gc-f (format nil "~a F~v$" (car gc-path) v f))
         (gcode nil))
    (dotimes (i nz) (push (append (list gc-z gc-f) (cdr gc-path)) gcode))
    (apply #'append gcode)))

|#
 
