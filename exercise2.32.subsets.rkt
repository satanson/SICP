(define (subsets s)
  (if (null? s)
      (list '())
      (let* (
            (head (car s))
            (subsets-excluding-head (subsets (cdr s))) 
            )
        (append subsets-excluding-head 
                (map (lambda (set) (cons head set)) subsets-excluding-head) )
        )
      )
  )

(subsets (subsets '(1 2 3)))