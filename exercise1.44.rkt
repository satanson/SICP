(load "util.rkt")
(define dx 0.0000001)
(define (smooth f)
  (lambda(x)
    (let (
          (x-dx (- x dx))
          (x+dx (+ x dx))
          (y1 (f x-dx))
          (y2 (f x+dx))
          (y  (f x)))
      (mean (list y1 y2 y)))))

(define (n-fold-smooth f)((repeated smooth) f))
                             