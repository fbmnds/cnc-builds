
(in-package #:paths/emitt)


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

(defun close-grouped-path (path)
  "Close the PATH of pairs using NCONC, if necessary."
  (let* ((len (length path))
         (first-c1 (caar path))
         (last-c1-c2 (nth (- len 1) path)))
    (if (c1-c2=  first-c1 (cdr last-c1-c2))
        path
        (nconc path (list (cons (cdr last-c1-c2) first-c1))))))

(defun close-path (path)
  "Close the PATH, if necessary."
  (if (member (first path) (last path) :test #'c=)
      path
      (let ((p2 (reverse path))) (nreverse (push (first path) p2)))))

(defun tag-path (path tags)
  "Tag the PATH elements TAGS where tags will be inserted."
  (let ((p2 (group-2 path))
        (ret nil))
    (labels ((c1c2= (s1 s2)
               (destructuring-bind ((a1 . b1) . (c1 . d1)) s1
                 (destructuring-bind ((a2 . b2) . (c2 . d2)) s2
                   (and (= a1 a2) (= b1 b2) (= c1 c2) (= d1 d2)))))
             (push-it (c1-c2)
               (if (member c1-c2 tags :test #'c1c2=)
                   (push (cons :tag c1-c2) ret)
                   (push c1-c2 ret))))
      (dolist (c1-c2 p2) (push-it c1-c2))
      (nreverse ret))))

(defun optimize-path (path &optional (eps 0.001))
  "Merge consecutive PATH segments recursively, iff they are collinear."
  (let ((c0 (car path))
        (c1 (cadr path))
        p)
    (if (and c0 c1)
        (labels ((rec (rpath)
                   (let ((c2 (car rpath)))
                     (if c2
                         (progn
                           (if (collinear-2d c0 c1 c2 eps)
                               (setf c1 c2)
                               (progn
                                 (push c0 p)
                                 (setf c0 c1)
                                 (setf c1 c2)))
                           (rec (cdr rpath)))
                         (progn
                           (push c0 p)
                           (push c1 p)
                           (nreverse p))))))
          (rec (cddr path)))
        path)))

(defun inner-ticks (r path)
  "Add ticks to inner corners to PATH."
  (let* ((c0 (car path))
         (c1 (cadr path))
         (p (list c0))
         (dr (* (- (sqrt 2) 1) r)))
    (if (and c0 c1)
        (labels ((rec (rpath)
                   (let ((c2 (cadr rpath)))
                     (push c1 p)
                     ;;(break)
                     (if c2
                         (progn
                           ;;(break)
                           (when (> (det2 (c- c2 c1) (c- c1 c0)) 0)
                             (push (c+ c1
                                       (c* dr (c-normed (c+ (normale-+ c2 c1)
                                                            (normale-+ c1 c0)))))
                                   p)
                             (push c1 p))
                           (setf c0 c1)
                           (setf c1 c2)
                           (rec (cdr rpath)))
                         (progn
                           (nreverse p))))))
          (rec (cdr path)))
        path)))

(defun segments-by-length (path)
  "Sort the segments of the copied PATH by length."
  (let ((p2 (group-2 (copy-list path))))
    (sort p2 #'(lambda (c1c2 c3c4)
                          (> (euklid (c- (car c1c2) (cdr c1c2)))
                             (euklid (c- (car c3c4) (cdr c3c4))))))))

(defun expand-path (path tags w/2 dz nz &optional (nz-pass 0))
  "Expand the PATH into a VECTOR of absolute XYZ coordinates, inserting 
tags at TAGS with width (* 2 W/2) and height (* |DZ| (- NZ NZ-PASS))."
  (let* ((path (tag-path (close-path path) tags))
         (l-path (length path))
         (l-tags (length tags))
         (l-vec (* 3 (+ 1 (* nz l-path) (* l-tags 4 (- nz nz-pass)))))
         (v (make-array l-vec :element-type 'float))
         (i 0))
    (flet ((c1-+w/2 (c1-c2 w/2)
             (let ((c1 (car c1-c2))
                   (len-c1-c2/2 (/ (euklid (c2-c1- c1-c2)) 2.0))
                   (c1-c2-normed (c-normed (c2-c1- c1-c2))))
               (cons (c+ c1 (c* (- len-c1-c2/2 w/2) c1-c2-normed))
                     (c+ c1 (c* (+ len-c1-c2/2 w/2) c1-c2-normed)))))
           (set-coord (v i x y z)
             (setf (aref v i) x)
             (setf (aref v (1+ i)) y)
             (setf (aref v (+ i 2)) z)))
      (if (eql :tag (caar path))
          (set-coord v 0 (caadar path) (cdadar path) 0.)
          (set-coord v 0 (caaar path) (cdaar path) 0.))
      (dotimes (iz0 nz)
        (dotimes (ip l-path)
          (setf i (+ 3 i))
          (let* ((path.ip (nth ip path))
                 (iz (1+ iz0))
                 (izdz (* iz dz)))
            (if (eql :tag (car path.ip))
                (let* ((c1-c2 (cdr path.ip))
                       (c1 (car c1-c2)))
                  (set-coord v i (car c1) (cdr c1) izdz)
                  (when (> iz nz-pass)
                    (destructuring-bind ((c1-w-x . c1-w-y) . (c1+w-x . c1+w-y))
                        (c1-+w/2 c1-c2 w/2)
                      (setf i (+ 3 i))
                      (set-coord v i c1-w-x c1-w-y izdz)
                      (setf i (+ 3 i))
                      (set-coord v i c1-w-x c1-w-y (* nz-pass dz))
                      (setf i (+ 3 i))
                      (set-coord v i c1+w-x c1+w-y (* nz-pass dz))
                      (setf i (+ 3 i))
                      (set-coord v i c1+w-x c1+w-y izdz))))
                (progn
                  (set-coord v i (caar path.ip) (cdar path.ip) izdz)))))))
    (values v (+ i 3))))

(defun convert-dxyz (c1-c2 i dz nz &optional (nt (/ nz 2)))
  "Convert a un-/tagged path segment into relative distances."
  (let* ((tag-p (eql :tag (car c1-c2)))
         (c-dz (round* (* i dz)))
         (c1-c2 (if tag-p (c2-c1- (cdr c1-c2)) (c2-c1- c1-c2)))
         (c-dx (c-x c1-c2))
         (c-dy (c-y c1-c2)))
    (if (and tag-p (> i (- nz nt)))
        (list (cons (cons 0. 0.) (* -1. c-dz))
              (cons (cons c-dx c-dy) 0.)
              (cons (cons 0. 0.) c-dz))
        (list (cons (cons c-dx c-dy) 0.)))))

(defun optimize-xy (path-dxyz)
  "Accumulate unidirectional X and Y movements, separately."
  (let ((c-d0 nil)
        (ret nil))
    (dolist (c-dxyz path-dxyz)
      (let* ((c-dxyz (round* c-dxyz))
             (c-dx (caar c-dxyz))
             (c-dy (cdar c-dxyz))
             (c-dz (cdr c-dxyz))
             (c-d0x (caar c-d0))
             (c-d0y (cdar c-d0))
             (c-d0z (cdr c-d0)))
        (cond ((null c-d0)
               (cond ((and (zerop* c-dx) (not (zerop* c-dy)))
                      (setf c-d0 c-dxyz))
                     ((and (zerop* c-dy) (not (zerop* c-dx)))
                      (setf c-d0 c-dxyz))
                     (t
                      (push c-dxyz ret))))
              (t
               (cond ((and (zerop* c-dx) (not (zerop* c-dy))
                           (zerop* c-d0x) (not (zerop* c-d0y))
                           (zerop* (- c-dz c-d0z)))
                      (setf c-d0 (cons (cons 0. (+ c-dy c-d0y)) c-dz)))
                     ((and (zerop* c-dx)
                           (not (zerop* c-dy)) (not (zerop* c-d0x)))
                      (push c-d0 ret)
                      (setf c-d0 c-dxyz))
                     ((and (zerop* c-dy) (not (zerop* c-dx))
                           (zerop* c-d0y) (not (zerop* c-d0x))
                           (zerop* (- c-dz c-d0z)))
                      (setf c-d0 (cons (cons (+ c-dx c-d0x ) 0.) c-dz)))
                     ((and (zerop* c-dy)
                           (not (zerop* c-dx)) (not (zerop* c-d0y)))
                      (push c-d0 ret)
                      (setf c-d0 c-dxyz))
                     (t
                      (push c-d0 ret)
                      (setf c-d0 nil)
                      (push c-dxyz ret)))))))
    (nreverse ret)))
                                                 
(defun optimize-relative-distances (path-dxyz &optional (eps 0.001))
  "Accumulate marginal and unidirectional movements."
  (let ((c-d0 nil)
        (c-d* (cons (cons 0. 0.) 0.))
        (ret nil))
    (dolist (c-dxyz path-dxyz)
      (let* ((c-dx (round* (caar c-dxyz)))
             (c-dy (round* (cdar c-dxyz)))
             (c-dz (round* (cdr c-dxyz)))
             (c-dx* (caar c-d*))
             (c-dy* (cdar c-d*))
             (c-dz* (cdr c-d*))
             (c-dxx* (+ c-dx c-dx*))
             (c-dyy* (+ c-dy c-dy*))
             (c-dzz* (+ c-dz c-dz*)))
        (cond ((and (> (abs c-dxx*) eps)
                    (> (abs c-dyy*) eps)
                    (> (abs c-dzz*) eps))
               (setf c-d* (cons (cons 0. 0.) 0.))
               (when c-d0
                 (push c-d0 ret)
                 (setf c-d0 nil))
               (push (cons (cons c-dxx* c-dyy*) c-dzz*) ret))
              ((and (> (abs c-dxx*) eps) (> (abs c-dyy*) eps))
               (setf c-d* (cons (cons 0. 0.) c-dzz*))
               (when c-d0
                 (push c-d0 ret)
                 (setf c-d0 nil))
               (push (cons (cons c-dxx* c-dyy*) 0.) ret))
              ((and (> (abs c-dxx*) eps) (> (abs c-dzz*) eps))
               (setf c-d* (cons (cons 0. c-dyy*) 0.))
               (when c-d0
                 (push c-d0 ret)
                 (setf c-d0 nil))
               (push (cons (cons c-dxx* 0.) c-dzz*) ret))
              ((and (> (abs c-dyy*) eps) (> (abs c-dzz*) eps))
               (setf c-d* (cons (cons c-dxx* 0.) 0.))
               (when c-d0
                 (push c-d0 ret)
                 (setf c-d0 nil))
               (push (cons (cons 0. c-dyy*) c-dzz*) ret))
              ((> (abs c-dxx*) eps)
               (cond ((null c-d0)
                      (setf c-d0 (cons (cons c-dxx* 0.) 0.))
                      (setf c-d* (cons (cons 0. c-dyy*) c-dzz*)))
                     ((and c-d0 (zerop* (cdar c-d0)) (zerop* (cdr c-d0)))
                      (setf c-d0 (cons (cons (+ c-dxx* (caar c-d0)) 0.) 0.))
                      (setf c-d* (cons (cons 0. c-dyy*) c-dzz*)))
                     (t 
                      (when c-d0
                        (push c-d0 ret)
                        (setf c-d0 nil))
                      (setf c-d0 (cons (cons c-dxx* 0.) 0.))
                      (setf c-d* (cons (cons 0. c-dyy*) c-dzz*)))))
              ((> (abs c-dyy*) eps)
               (cond ((null c-d0)
                      (setf c-d0 (cons (cons 0. c-dyy*) 0.))
                      (setf c-d* (cons (cons c-dxx* 0.) c-dzz*)))
                     ((and c-d0 (zerop* (caar c-d0)) (zerop* (cdr c-d0)))
                      (setf c-d0 (cons (cons 0. (+ c-dyy* (cdar c-d0))) 0.))
                      (setf c-d* (cons (cons c-dxx* 0.) c-dzz*)))
                     (t
                      (when c-d0
                        (push c-d0 ret)
                        (setf c-d0 nil))
                      (setf c-d0 (cons (cons 0. c-dyy*) 0.))
                      (setf c-d* (cons (cons c-dxx* 0.) c-dzz*)))))
              ((> (abs c-dzz*) eps)
               (when c-d0
                 (push c-d0 ret)
                 (setf c-d0 nil))
               (setf c-d* (cons (cons c-dxx* c-dyy*) 0.))
               (push (cons (cons 0. 0.) c-dzz*) ret))
              (t
               (setf c-d* (cons (cons c-dxx* c-dyy*) c-dzz*))))))
    (when c-d0 (push (c+ c-d0 c-d*) ret))
    (reverse ret)))

(defun convert-path-dxyz% (path tags w/2 dz nz &optional (nt (/ nz 2)))
  "Convert a PATH with TAGS into relative distances ((DX . DY) . DZ)."
  (let ((p2 (insert-tags path tags w/2))
        (ret nil))
    (dotimes (i nz)
      (when (> i 0)
        (push (cons (cons 0. 0.) dz) ret)
        (dolist (p-c1-c2 p2)
          (dolist (dxyz-c1-c2 (convert-dxyz p-c1-c2 i dz nz nt))
            (push dxyz-c1-c2 ret)))))
    (nreverse ret)))

(defun convert-path-dxyz (path tags w/2 dz nz
                          &optional (nt (/ nz 2)) (eps 0.001))
  "Convert a PATH with TAGS into optimized relative distances ((DX . DY) . DZ)."
  (declare (ignore eps))
  (optimize-xy (convert-path-dxyz% path tags w/2 dz nz nt)))

(defun emitt-gcode-xy-z (path dz f &optional (fz (* 0.8 f)) (v 3))
  (let ((c0-z (c-z (car path)))
        (ret nil))
    (push (format nil "G0 Z~v$ F~v$" v c0-z v fz) ret)
    (dolist (c (cdr path))
      (if (c= (cons (cons 0. 0.) dz) c)
          (progn
            (push (format nil "G0 Z~v$ F~v$" v dz v fz) ret)
            (setf c0-z (c-z c)))
          (let ((gc "")
                (c-x (c-x c))
                (c-y (c-y c))
                (c-z (c-z c)))
            (cond ((< (round* (* c-z dz)) 0.)
                   (setf gc (format nil "Z~v$ F~v$" v c-z v fz)))
                  ((> (round* (* c-z dz)) 0.)
                   (setf gc (format nil "Z~v$" v c-z)))
                  ((> (round* (* c0-z dz)) 0.)
                   (setf gc (format nil "F~v$" v f)))
                  (t :ignore))
            
            (unless (zerop* c-y) (setf gc (format nil "Y~v$ ~a" v c-y gc)))
            (unless (zerop* c-x) (setf gc (format nil "X~v$ ~a" v c-x gc)))
            (let ((gc (string-trim '(#\ ) (format nil "G0 ~a" gc))))
              (if (string= gc "G0")
                  (print c)
                  (progn
                    (setf c0-z c-z)
                    (push gc ret)))))))
    (nreverse ret)))

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
 
