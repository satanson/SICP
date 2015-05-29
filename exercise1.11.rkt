(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3)))
         )
      )
  )

(define (f-iterative n )
  (if (< n 3)
      n
      (f-iter 2 n 0 1 2)
      )
  )

(define (f-iter i n a0 a1 a2)
  (if (= i n)
       a2
      (f-iter (+ i 1) n
               a1
               a2
              (+ a2 (* 2 a1 ) (* 3 a0)))
      )
  )

(define (foldr f z xs)
  (cond ((null? xs) z)
        ((f (car xs)
            (foldr f z (cdr xs))
         ))
  )
)
(define (range args)
  (let 
   
(define fi f-iterative)
(define fr f-recursive)
    