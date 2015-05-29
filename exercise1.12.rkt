(define (pascal-triangle n m)
  (cond ((or (= m 1) (= m n)) 1)
        ((+ (pascal-triangle (- n 1) (- m 1))
            (pascal-triangle (- n 1) m)
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
(define (show n)
  (cond
    ((= n 0) '())
    ((reverse (cons (let ((p (lambda (x)(pascal-triangle n x)))) 
                         
                      (map p  (range 1 (+ n 1)))
                    )
                    (reverse (show (- n 1)))
              )
    ))
   )
)