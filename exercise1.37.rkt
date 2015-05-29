(load "util.rkt")
(define (cont-frac n d k)
  (define (cont-frac-impl i)
    (if (> i k) 0
        (let (
              (N (n i))
              (D (+ (d i) (cont-frac-impl (+ 1 i)))))
          (/ N D))))
  (cont-frac-impl 1))


(define (fai n) (cont-frac (lambda(x) 1.0) (lambda (x) 1.0) n))
(map (lambda (n) (fai (expt 10 n))) (range 1 6))

(define (cont-frac2 n d k)
  (define (iter i result)
    (if (zero? i)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (fai2 n) (cont-frac2 (lambda(x) 1.0) (lambda (x) 1.0) n))
(map (lambda (n) (fai2 (expt 10 n))) (range 1 6))
        
        