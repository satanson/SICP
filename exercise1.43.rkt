(load "util.rkt")
(define (repeated f n)
  (lambda (x)
    (define (repeated-iter k result)
      (if (zero? k)
          result
          (repeated-iter (- k 1) (f result))))
    (repeated-iter n x)))

(define (repeated2 f n)
  (define (repeated2-iter n g)
    (if (zero? n)
        g
        (repeated2-iter (- n 1) (compose f g))))
  (repeated2-iter n id))

(define (add n m)((repeated inc n) m))
((repeated square 2) 5)
((repeated2 inc 2) 5)
((repeated2 square 2) 5)