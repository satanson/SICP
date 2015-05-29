(define (fast-exp b n)
  (fast-exp-iter b n 1))

(define (fast-exp-iter b n e)
  (cond
    ((= n 1)
     (* b e))
    ((fast-exp-iter 
      (square b) 
      (quotient n 2) 
      (* e (if (= (modulo n 2) 0) 1 b))
      ))
    )
  )

(define range (lambda x (range-args x)))

(define (range-args args)
  (cond
    ((= (length args) 1) (range-impl 0 (car args) 1))
    ((= (length args) 2) (range-impl (car args) (cadr args) 1))
    ((= (length args) 3) (range-impl (car args) (cadr args) (caddr args)))
    )
  )

(define (range-impl b e s)
  (cond
    ((< b e) (cons b (range-impl (+ b s) e s)))
    ('())
  )
)
(define (square x) (* x x))
(map (lambda(k) (* (fast-exp 2 (- 20 k)) (fast-exp 2 k))) (range 1 20))