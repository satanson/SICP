(load "util.rkt")

(define (product item a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (item a)))))
  (product-iter a 1))

(define (quarter-pi-item i)
  (let (
        (a (* 2 i 2 (+ i 1)))
        (b (square (+ 1 (* 2 i)))))
    (/ a b)))

(define (pi n)
  (+ 0.0 (* 4 (product quarter-pi-item 1 inc n))))