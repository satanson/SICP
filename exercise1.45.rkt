(load "util.rkt")
(define (nth-root-try x n m)
  (let* (
        (f (lambda (y) (/ x (expt y (- n 1)))))
        (g (lambda (y) ((repeated average-damp m) f))))
    (fixed-point-of-transform f g 1.0))) 

(define (ceilinglog n) 
  (define (iter k)
    (if (> (expt 2 k) n)
        (- k 1)
        (iter (+ k 1))))
  (iter 2))

;(nth-root-try 256 8 2)
;(map (lambda(n) (list n (ceilinglog n))) (range 1 100))

(define (nth-root x n)
  (cond 
    ((= n 0) 1)
    ((= n 1) x)
    (else (nth-root-try x n (ceilinglog n)))))

(map (lambda(n) (nth-root (expt 2 n) n)) (range 0 13))