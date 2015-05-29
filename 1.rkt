(call-with-current-continuation
  (lambda (k)
    (for-each (lambda (x)
                (if (negative? x)
                    (k x)))
              '(54 0 -37 3 245 19))
    #t)) 
(define call/cc call-with-current-continuation)

(define product
  (lambda (ls)
    (call/cc
      (lambda (break)
        (let f ((ls ls))
          (cond
            ((null? ls) 1)
            ((= (car ls) 0) (break 0))
            (else (* (car ls) (f (cdr ls))))))))))

(((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!")

(define retry #f) 
(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))