(load "util.rkt")
(define (horner-eval x coeff-seq)
  (define (f coeff result) (+ coeff (* x result)))
  (foldr f 0 coeff-seq))

(horner-eval 2 (list 1 3 0 5 0 1))

(horner-eval 2 (list 1 1 1 1 1 1 1 1))

(horner-eval 2 (list 0 0 0 0 0 0 0 0 1))