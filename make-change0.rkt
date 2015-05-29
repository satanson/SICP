(define (pos i array)
  (if (= i 0)
      (car array)
      (pos (- i 1) (cdr array))
  )
)

(define (len array)
  (if (null? array)
      0
      (+ 1 (len (cdr array)))
   )
  )

;(define coins '(1 5 10 25 50))
  
(define (make-change-with-coins n coins)
  (cond ((< n 0) 0)
        ((= n 0) 1)
        ((= (car coins) 1) 1)
        ((make-change-with-coins-iter n coins 0))
   )
 )

(define (make-change-with-coins-iter n coins sum-range)
  (if (null? coins) 
       sum-range
      (make-change-with-coins-iter n 
                                   (cdr coins) 
                                   (+ sum-range (make-change-with-coins (- n (car coins)) coins))
      )
   )
 )

(define (make-change n ) (make-change-with-coins n '(50 25 10 5 1)))

(make-change 10)

;(define (fib n) (make-change-with-coins n '(2 1)))