(define (double x) (+ x x))
(define (halve x)(quotient x 2))
(define (min-max x y)
  (if (< x y)
      (list x y)
      (list y x)
      )
  )

(define (sign x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        ((> x 0) 1)
        )
  )

(define (homo-sign? x y)
  (cond
    ((or (and (< x 0) (< y 0)
              (> x 0) (> y 0))
         )
     #t)
    ((or (and (< x 0) (> y 0)
              (> x 0) (< y 0))
         )
     #f)
    )
  )

;Russian Peasant Method
(define (mul a b)
  (cond 
    ((or (= a 0) (= b 0))
     0)
    ((homo-sign? a b)
     (apply mul-iter `(,@(min-max (abs a) (abs b)) 0)))
    ((- (apply mul-iter `(,@(min-max (abs a) (abs b) 0)))))
    )
  )
     

(define (mul-iter b n s)
  (cond
    ((= n 1)
     (+ b s))
    ((mul-iter (double b) 
               (halve n)
               (+ s (if (even? n) 0 b))
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