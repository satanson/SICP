(define (make-change-with-coins n coins)
  (cond ((< n 0) 0)
        ((= n 0) 1)
        ((null? coins) 0)
        ((+ (make-change-with-coins (- n (car coins)) coins)
            (make-change-with-coins n (cdr coins))))
   ))

(define (make-change n) (make-change-with-coins n '(1 5 10 25 50)))