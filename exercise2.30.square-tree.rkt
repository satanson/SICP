(load "util.rkt")
(define (square-tree tree)
  (define (f node xs) 
    (if (list? node)
        (cons (square-tree node) xs)
        (cons (square node) xs)
        )
    )
  (foldr f '() tree)
  )

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))