(define a '(1 2 3 4))
(set-cdr! a '(1 2 3 5 6 6))

(define (my_append list e)
  (if (null? list) (cons e '())
         (set-cdr! list (my_append (cdr list) e))))


(define (setcdr x y) (set-cdr! x y))


;; in Scheme,
(define (duplicate ls)
  (letrec ((head (list 1))
           (dup_aux (lambda (ls end)
      (if (not (null? ls))
        (begin 
          (set-cdr! end (list (car ls)))
          (dup_aux (cdr ls) (cdr end)))))))
    (dup_aux ls head) 
    (cdr head)))
