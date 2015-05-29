(load "util.rkt")
(define (sum-range term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-range id 0 inc 10)