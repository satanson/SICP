(load "util.rkt")

(define (fixed-point2 f x)((iterative-improve close-enough? f) x))
(define (sqrt6 x)(fixed-point2 (lambda(y) (mean (list y (/ x y)))) 1.0))
(sqrt6 2)
(sqrt6 3)
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      