(define (mul b n)
  (mul-iter b n 0))

(define (mul-iter b n e)
  (cond
    ((= n 1)
     (+ b e))
    ((mul-iter 
      (double b) 
      (quotient n 2) 
      (+ e (if (= (modulo n 2) 0) 0 b))
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
(define (double x) (+ x x))
(map (lambda(k) (mul (expt 2 (- 20 k)) (expt 2 k))) (range 1 20))