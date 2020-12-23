(defparameter lx 376)
(defparameter d-lx 24.5)
(defparameter d-lx-2 327)
(assert (= d-lx-2 (- lx (* 2 d-lx))))

(defparameter ly 250)
(defparameter d-ly 18.6)
(defparameter d-ly-1 231.4)
(assert (= d-ly-1 (- ly d-ly)))

(defparameter r 6)

(defparameter hp-keyb
  (list (cons r 0) (cons (- lx r) 0)
        (cons lx r) (cons lx (- ly r))
        (cons (- lx r) ly)
        (cons (- lx d-lx) ly)
        (cons (- lx d-lx) (- ly d-ly))
        (cons d-lx (- ly d-ly)) (cons d-lx ly)
        (cons r ly)
        (cons 0 (- ly r)) (cons 0 r)))



