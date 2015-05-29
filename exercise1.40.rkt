(load "util.rkt")
(define (cubic a b c)
  (lambda (x)
    (let (
          (n3 (cube x))
          (n2 (* a (square x)))
          (n1 (* b x)))
      (+ n3 n2 n1 c))))
(map (cubic 1 1 1) (range 1 10))

(newton-method (cubic 1 1 1) 1.0)