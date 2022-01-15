
(in-package #:paths)


(defparameter *precision* 0.000001)
(defparameter *eps* 0.001)

(defun set-precision (value) (setf *precision* value))
(defun set-machine-precision (value) (setf *eps* value))

;; On Lisp, pp. 47, 219, 410(?)

(defun group-3 (l)
  "Group the list L in triples, wrap around the first two items.

Example: (group-3 '(1 2 3)) => ((1 2 3) (2 3 1) (3 1 2))

Close a path, i.e. a list of (X . Y) coordinates, with
the first path segment attached to the end of the path.
This allows to consider the corner around the first path point
when calculating the inner/outer path."
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
  "Group the list L in conses, wrap around the first element.

Example: (group-2 '(1 2 3)) => ((1 . 2) (2 . 3) (3 . 1))

Convert a path, i.e. a list coordinates, into a list of path segments, 
i.e. list of coordinate tuples for further processing."
  (let ((head (car l)))
    (labels ((rec (l acc)
               (cond ((and (car l) (cadr l))
                      (rec (cdr l)
                           (push (cons (car l) (cadr l)) acc)))
                     (t (nreverse (cons (cons (cdar acc) head) acc))))))
      (when l (rec l nil)))))

(defun round* (x)
  "Round numbers, XY-/XYZ-coordinates and lists thereof."
  (cond ((numberp x) (* (round x *precision*) *precision*))
        ((and (consp x) (eql :tag (car x))) (round* (cdr x)))
        ((and (listp x) (consp (car x)) (consp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        ((and (consp x) (numberp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        ((and (consp x) (consp (car x)) (numberp (cdr x)))
         (cons (round* (car x)) (round* (cdr x))))
        (t (error (format nil "ROUND* undefined for ~a" x)))))

;; deprecated
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun expand-call (type expr)
    `(,(car expr) ,@(mapcar #'(lambda (a)
                                `(with-type ,type ,a))
                            (cdr expr)))))

;; deprecated
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun binarize (expr)
    (if (and (nthcdr 3 expr)
             (member (car expr) '(+ - * /)))
        (destructuring-bind (op a1 a2 . rest) expr
          (binarize `(,op (,op ,a1 ,a2) ,@rest)))
        expr)))

;; deprecated
(defmacro with-type (type expr)
  `(the ,type ,(if (atom expr)
                   expr
                   (expand-call type (binarize expr)))))

;; deprecated
(defun poly (a b x)
  (with-type fixnum (+ (* a (expt x 2)) (* b x))))

(defun xy-p (c) (and (consp c) (numberp (car c)) (numberp (cdr c))))
(defun xyz-p (c) (and (consp c) (xy-p (car c)) (numberp (cdr c))))
(defun tag-xyz-p (c) (and (consp c) (xy-p (car c)) (numberp (cdr c))))

(deftype coord-xy () '(satisfies xy-p))
(deftype coord-xyz () '(satisfies xyz-p))
(deftype coord-tag-xyz () '(satisfies tag-xyz-p))
(deftype coord () '(or coord-xy coord-xyz))

(defun xy-xy-p (c) (and (consp c) (xy-p (car c)) (xy-p (cdr c))))
(defun xyz-xyz-p (c) (and (consp c) (xyz-p (car c)) (xyz-p (cdr c))))

(deftype segment-xy () '(satisfies xy-xy-p))
(deftype segment-xyz () '(satisfies xyz-xyz-p))
(deftype segment () '(or segment-xy segment-xyz))

(defun c-x (c)
  "Return X of XY- and XYZ-coordinates."
  (typecase c
    (coord-xy (round* (car c)))
    (coord-xyz (c-x (car c)))
    (t (error (format nil "C-X undefined for ~a" c)))))

(defun c-y (c)
  "Return Y of XY- and XYZ-coordinates."
  (typecase c
    (coord-xy (round* (cdr c)))
    (coord-xyz (c-y (car c)))
    (t (error (format nil "C-Y undefined for ~a" c)))))

(defun c-z (c)
  "Return Z of XYZ-coordinates."
  (typecase c
    (coord-xyz (round* (cdr c)))
    (t (error (format nil "C-Z undefined for ~a" c)))))

(defun test-c-z (c)
  "Return the Z value of the XY-/XYZ-coordinate C, iff it is 'above' EPS."
  (typecase c
    (coord-xyz (let ((it (c-z c))) (when (> (abs it) *eps*) it)))
    (coord-xy nil)
    (t (error (format nil "TEST-C-Z undefined for ~a" c)))))

(defun c- (c1 c2)
  "Return the difference vector C1 - C2 of XY-/XYZ-coordinates."
  (cond ((and (xy-p c1) (xy-p c2))
         (cons (- (c-x c1) (c-x c2)) (- (c-y c1) (c-y c2))))
        ((and (xyz-p c1) (xyz-p c2))
         (cons (cons (- (c-x c1) (c-x c2))
                     (- (c-y c1) (c-y c2)))
               (- (c-z c1) (c-z c2))))
        (t (error (format nil "C- undefined for ~a ~a" c1 c2)))))

(defun c2-c1- (c1-c2)
  "Return the difference vector C1 - C2 of a path segment of 
XY-/XYZ-coordinates."
  (typecase c1-c2
    (segment (c- (cdr c1-c2) (car c1-c2)))
    (t (error (format nil "C2-C1- undefined for ~a" c1-c2)))))

(defun c+ (c1 c2)
  "Return the sum vector C1 + C2 of XY-/XYZ-coordinates."
  (cond ((and (xy-p c1) (xy-p c2))
         (cons (+ (c-x c1) (c-x c2)) (+ (c-y c1) (c-y c2))))
        ((and (xyz-p c1) (xyz-p c2))
         (cons (cons (+ (c-x c1) (c-x c2))
                     (+ (c-y c1) (c-y c2)))
               (+ (c-z c1) (c-z c2))))
        (t (error (format nil "C+ undefined for ~a ~a" c1 c2)))))

(defun c* (r c)
  "Return the linear product R * C for XY-/XYZ-coordinates."
  (cond ((and (numberp r) (xy-p c))
         (cons (round* (* r (c-x c))) (round* (* r (c-y c)))))
        ((and (numberp r) (xyz-p c))
         (cons (cons (round* (* r (c-x c))) (round* (* r (c-y c))))
               (round* (* r (c-z c)))))
        (t (error (format nil "C* undefined for ~a ~a" r c)))))

(defun euklid (c)
  "Return the euklidian norm/length of the XY-/XYZ-coordinate C."
  (typecase c
    (coord-xy (round* (sqrt (+ (* (c-x c) (c-x c))
                               (* (c-y c) (c-y c))))))
    (coord-xyz (round* (sqrt (+ (* (c-x c) (c-x c))
                                (* (c-y c) (c-y c))
                                (* (c-z c) (c-z c))))))
    (t (error (format nil "EUKLID undefined for ~a" c)))))

(defun zerop* (x &optional (precision *precision*))
  (typecase x
    (number (> precision (abs x)))
    (coord (> precision (euklid x)))
    (segment (> precision (euklid (c- (cdr x) (car x)))))
    (t (error (format nil "ZEROP* undefined for ~a" c)))))

(defun c= (c1 c2)
  "Return numerical equality for numbers, XY-/XYZ-coordinates and paths."
  (cond ((and (numberp c1) (numberp c2))
         (> *precision* (abs (- c1 c2))))
        ((and (typep c1 'coord) (typep c2 'coord))
         (> *precision* (euklid (c- c1 c2))))
        ((and (listp c1) (listp c2))
         (every #'identity (mapcar #'c= c1 c2)))
        (t (error (format nil "c= undefined for ~a ~a" c1 c2)))))

(defun c1-c2= (s1 s2)
  "Return numerical equality for XY-/XYZ path segments."
  (and (c= (car s1) (car s2)) (c= (cdr s1) (cdr s2))))

(defun c-normed (c)
  "Return the normed vector of the XY-/XYZ-coordinate C."
  (typecase c
    (coord (c* (/ 1. (euklid c)) c))
    (t (error (format nil "C-NORMED undefined for ~a" c)))))

(defun normale-+ (c1 c2)
  "Return the positive oriented normale of the XY-/XYZ path segment C1 - C2."
  (let ((d (c-normed (c- c1 c2))))
    (cond ((c= c1 c2) (error "undefined normale on zero vector"))
          (t (round* (cons (* -1.0 (c-y d)) (c-x d)))))))

(defun normale-- (c1 c2)
  "Return the negative oriented normale of the XY-/XYZ path segment C1 - C2."
  (c* -1.0 (normale-+ c1 c2)))

(defun det2 (c1 c2)
  "Return the determinante of the XY-coordinates C1 and C2."
  (round* (- (* (c-x c1) (c-y c2)) (* (c-x c2) (c-y c1)))))

(defun collinear-coord-p (c0 c1 c2)
  (if (or (and (typep c0 coord-xy) (typep c1 coord-xy) (typep c2 coord-xy))
          (and (typep c0 coord-xyz) (typep c1 coord-xyz) (typep c2 coord-xyz)))
      (let ((c1-c0 (c-normed (c- c1 c0)))
        (c2-c0 (c-normed (c- c2 c0))))
        (< (euklid (c- c1-0 c2-c0)) *precision*))
      (error
       (format nil "COLLINIEAR-COORD-P undefined for ~a ~a ~a" c0 c1 c2))))

(defun collinear-segment-p (s0 s1)
  (cond ((and (or (and (typep s0 segment-xy) (typep s1 segment-xy))
                  (and (typep s0 segment-xyz) (typep s1 segment-xyz)))
              (c= (cdr s0) (car s1)))
         (collinear-coord-p (car s0) (cdr s0) (cdr s1)))
        ((or (and (typep s0 segment-xy) (typep s1 segment-xy))
             (and (typep s0 segment-xyz) (typep s1 segment-xyz)))
         (< (euklid (c-normed (c- (cdr s0) (car s0)))
                    (c-normed (c- (cdr s1) (car s1))))
            *precision*))
        (t (error
            (format nil "COLLINIEAR-SEGMENT-P undefined for ~a ~a" s0 s1)))))

;; deprecated
(defun collinear-2d (c0 c1 c2)
  "Return the segment (C0 . C2), iff the XY-coordinates C0, C1 and C2 are collinear."
  (let ((x1 (car c0))
        (y1 (cdr c0))
        (x2 (car c1))
        (y2 (cdr c1))
        (x3 (car c2))
        (y3 (cdr c2)))
    (when (> *precision*
             (abs (+ (* x1 (- y2 y3))
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
  "Return the coordinate of the inner XY point with distance R
to both path segments C1 C2 and C2 C3. The distance to the corner C1 C2 C3
is (* SQRT 2 R), iff the segments are perpendicular to each other.

Example: (shift-corner-+ 1. '(0 . 0) '(1 . 0) '(1 . 1)) => (0.0 . 1.0)"
  (let* ((n1 (normale-+ c2 c1))
         (n2 (normale-+ c3 c2))
         (c-1-2 (c- c1 c2))
         (det (det2 c-1-2 (c- c3 c2)))
         (l (/ (* r (+ (* (- (cdr c3) (cdr c2)) (- (car n1) (car n2)))
                       (* (- (car c2) (car c3)) (- (cdr n1) (cdr n2)))))
               det)))
    (c+ (c* r n1) (c- c2 (c* l c-1-2)))))

(defun shift-corner-- (r c1 c2 c3)
  "Return the coordinate of the outer XY point with distance R
to both path segments C1 C2 and C2 C3. The distance to the corner C1 C2 C3
is (* SQRT 2 R), iff the segments are perpendicular to each other.

Example: (shift-corner-+ 1. '(0 . 0) '(1 . 0) '(1 . 1)) => (0.0 . 1.0)"
  (let* ((n1 (normale-- c2 c1))
         (n2 (normale-- c3 c2))
         (c-1-2 (c- c1 c2))
         (det (det2 c-1-2 (c- c3 c2)))
         (l (/ (* r (+ (* (- (cdr c3) (cdr c2)) (- (car n1) (car n2)))
                       (* (- (car c2) (car c3)) (- (cdr n1) (cdr n2)))))
               det)))
    (c+ (c* r n1) (c- c2 (c* l c-1-2)))))

(defun in-row-p (c)
  "Return T iff either the X or the Y coordinates are numerically equal for the first
three coordinates of the path C."
  (ignore-errors (or (= (caar c) (caadr c) (caaddr c))
                     (= (cdar c) (cdadr c) (cdaddr c)))))

(defun stats (p)
  "Return avg-min-max-length-statistics for the path P of XY-coordinates."
  (loop for c in p
        counting c into len
        maximizing (c-x c) into max-x
        minimizing (c-x c) into min-x
        summing (c-x c) into avg-x
        maximizing (c-y c) into max-y
        minimizing (c-y c) into min-y
        summing (c-y c) into avg-y
        finally
           (return
             (list :len len
                   :max-x max-x :min-x min-x :avg-x (round* (/ avg-x len))
                   :max-y max-y :min-y min-y :avg-y (round* (/ avg-y len))))))

(defun stats-acc (ps)
  "Return the list of avg-min-max-length-statistics for the list of paths PS
of XY-coordinates."
  (loop for s in (mapcar #'stats ps)
        summing (getf s :len) into len
        maximizing (getf s :max-x) into max-x
        minimizing (getf s :min-x) into min-x
        summing (* (getf s :len) (getf s :avg-x)) into avg-x
        maximizing (getf s :max-y) into max-y
        minimizing (getf s :min-y) into min-y
        summing (* (getf s :len) (getf s :avg-y)) into avg-y
        finally
           (return
             (list :len len
                   :max-x max-x :min-x min-x :avg-x (round* (/ avg-x len))
                   :max-y max-y :min-y min-y :avg-y (round* (/ avg-y len))))))

(defun trim-path (p &optional (len (length p)))
  "Deduplicate the XY-path P and reduce those path segments which are in row with
regard to either the X- or Y-coordinate."
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
  "Return the inner/outer path with distance R to the path L, depending on
the orientation of L."
  (remove-if #'null
             (mapcar #'(lambda (c)
                         (ignore-errors
                          (shift-corner-+ r (car c) (cadr c) (caddr c))))
                     (group-3 (trim-path l)))))

(defun shift-path-- (r l)
  "Return the outer/inner path with distance R to the path L, depending on
the orientation of L."
  (remove-if #'null
             (mapcar #'(lambda (c)
                         (ignore-errors
                          (shift-corner-- r (car c) (cadr c) (caddr c))))
                     (group-3 (trim-path l)))))

(defun circle-path (r n)
  "Return the circle XY-path of N segments with radius R around the origin."
  (let ((pi2/n (/ (* 2 pi) n)))
    (loop for i from 0 to (1- n) collect (cons (* r (cos (* i pi2/n)))
                                               (* r (sin (* i pi2/n)))))))

(defun min-xy-path (path)
  "Return (MIN X) . (MIN Y) of XY-path PATH.

This function is redundant to STATS."
  (reduce
   #'(lambda (c d) (cons (min (car c) (car d)) (min (cdr c) (cdr d))))
   (cdr path) :initial-value (cons (caar path) (cdar path))))

(defun flip-path (path)
  "Return the path of flipped YX-coordinates of PATH."
  (mapcar #'(lambda (c) (cons (cdr c) (car c))) path))

(defun shift-path-origin (path &optional (offset (cons 0. 0.)))
  "Shift the XY-path PATH such that the origin intuitively marks the lower left corner.

The returned path is aligned with the axis X=0 and Y=0 in the upper right quadrant."
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
  "Return the path segment increment in polar coordinates for the spiral
around the origin from S0-POLAR to S1-POLAR. The approximation quality 
is determined by DXY as the length of the returned path segment."
  (destructuring-bind (r0 . alfa0) s0-polar
    (destructuring-bind (r1 . alfa1) s1-polar
      (let ((n (round (/ (radian s0-polar s1-polar eps) dxy)))
            (dalfa (mod (- alfa1 alfa0) (* 2 pi))))
        (if (> eps dalfa)
            (cons n (cons (/ (- r1 r0) n) (/ (* 2 pi) n)))
            (cons n (cons (/ (- r1 r0) n) (/ dalfa n))))))))

(defun spiral-polar (dxy s0-polar s1-polar &optional (eps 0.00001))
  "Return the path in polar coordinates for the spiral
around the origin from S0-POLAR to S1-POLAR. The approximation quality 
is determined by DXY as the length of the returned path segments."
  (destructuring-bind (r0 . alfa0) s0-polar
    (destructuring-bind (n . (dr . dalfa)) (n-d-polar dxy s0-polar s1-polar eps)
      (let ((spiral '()))
        (dotimes (i n)
          (push (cons (+ (* i dr) r0) (+ (* i dalfa) alfa0)) spiral))
        (push s1-polar spiral)
        (nreverse spiral)))))

(defun spiral (dxy s0 s1 &optional (eps 0.00001))
  "Return the XY-path in cartesian coordinates for the spiral
around the origin from cartesian S0 to cartesian S1. The approximation quality 
is determined by DXY as the length of the returned path segments."
  (let ((s0-polar ( cartesian->polar s0))
        (s1-polar ( cartesian->polar s1)))
    (mapcar #'polar->cartesian
            (spiral-polar dxy s0-polar s1-polar eps))))

(defun closed-spiral (dxy s0 s1 &optional (eps 0.00001))
  "Return the XY-path in cartesian coordinates for the spiral
around the origin from catesion S0 to cartesian S1 enclosed in the outer circle.
The approximation quality is determined by DXY as the length of the returned
path segments."
  (nconc (spiral dxy s0 s1 eps)
         (spiral dxy s1 s1 eps)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun nquicksort (vec l r &key
                             (value #'(lambda (x) (svref vec x)))
                             (test #'(lambda (x y)
                                       (cond ((< x y) t)
                                             ((> x y) nil)
                                             (t :eq)))))
  
  (let* ((i l)
         (j r)
         (p (round (/ (+ l r) 2)))
         (vec/p (funcall value p)))
    (while (<= l i j r)
      (while (and (< i (1+ r))
                  (funcall test (funcall value i) vec/p))
        (incf i))
      (when (= i (1+ r))
        (dolist (k (list p (1- r)))
          (setf (svref vec k) (funcall value (1+ k))))
        (setf (svref vec r) vec/p)
        (setf i (1- r)))
      (while (and (> j l)
                  (not (funcall test (funcall value j) vec/p)))
        (decf j))
      (when (= j l)
        (dolist (k (list (1+ l) p))
          (setf (svref vec k) (funcall value (1- k))))
        (setf (svref vec l) vec/p)
        (setf j (1+ l)))
      (when (<= l i j r)
        (let ((temp (funcall value i)))
          (setf (svref vec i) (funcall value j))
          (setf (svref vec j) temp))
        (incf i)
        (decf j)))
    (nquicksort vec l (max j l) :value value :test test)
    (nquicksort vec (min (1+ (max j l)) r) r :value value :test test))
  vec)


