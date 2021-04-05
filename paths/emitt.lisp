
(in-package #:paths/emitt)


(defun group-2 (l)
  (labels ((rec (l acc)
             (cond ((and (car l) (cadr l))
                    (rec (cdr l)
                         (cons (list (car l) (cadr l)) acc)))
                   (t (nreverse acc)))))
    (when l (rec l nil))))

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

(defun emitt-gcode (path &optional (v 3) (eps 0.001))
  (remove-if #'null
             (mapcar #'(lambda (l)
                         (let ((c-dx (- (caadr l) (caar l)))
                               (c-dy (- (cdadr l) (cdar l))))
                           (cond ((and (< eps (abs c-dx)) (< eps (abs c-dy)))
                                  (format nil "G0 X~v$Y~v$" v c-dx v c-dy))
                                 ((< eps (abs c-dx))
                                  (format nil "G0 X~v$" v c-dx))
                                 ((< eps (abs c-dy))
                                  (format nil "G0 Y~v$" v c-dy))
                                 (t nil))))
                     (group-2 path))))

 
