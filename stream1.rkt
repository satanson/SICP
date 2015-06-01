#lang racket
(require r5rs)
(require "stream.rkt")
;(s-println (s-take integers 10))

(define (integral delayed-integrand initial-value dt)
  (let int []
    (s-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (s-add (s-scale integrand dt)
                                (int))))))
{comment
(define (RC R C dt)
  (define (voltage v0 i)
    (s-add (s-scale (integral i v0 dt) (/ 1 C)) (s-scale i R)))
  voltage)

(define RC1 (RC 5 1 0.5))

(define (sign-change-detector a b)
  (cond ((and (> 0 b) (<= 0 a)) 1)
        ((and (> 0 a) (<= 0 b)) -1)
        (else 0)))

(define (zero-crossings sense-data)
  (s-map sign-change-detector sense-data (s-cons 0 sense-data)))

(define k
  (s-cons 1 k))
}
;(s-println (s-take k 10))
(define (solve f y0 dt)
  (define y (integral (delay (s-map f y)) y0 dt))
  ;(define dy (s-map f y))
  y)

(define square (lambda[x]{* x x}))
;(solve square 0 0.001) 

;(s-nth (solve (lambda (y) y) 1 0.001) 1000)
;2.716924

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (s-map accum (s-range 1 20)))
(define y (s-filter even? seq))
(define z (s-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(s-nth y 7)
(s-println z)
(define seq2 (s-take (partial-sums pos-integers) 20))
(define y2 (s-filter even? seq2))
(define z2 (s-filter (lambda(x) (= (remainder x 5) 0))
                     seq2))
(s-nth y2 7)
(s-println z2)
