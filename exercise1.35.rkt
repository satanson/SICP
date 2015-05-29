(load "util.rkt")
(define fai (+ 0.0 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)))
fai