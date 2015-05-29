(load "util.rkt")
(define (quarter-pi-item i)
  (let (
        (a (* 2 i 2 (+ i 1)))
        (b (square (+ 1 (* 2 i)))))
    (/ a b)))

(define (pi n) (+ 0.0 (* 4 (prod quarter-pi-item 1 inc n))))
;(map (lambda (i)(pi (expt 10 i))) (range 1 5))