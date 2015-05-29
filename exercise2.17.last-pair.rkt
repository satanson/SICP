(load "util.rkt")
;recursive
(define (last-pair-v1 xs)
  (if (null? xs)
      nil
      (list (car (reverse xs)))))

(last-pair-v1 nil)
(last-pair-v1 (list 1))
(last-pair-v1 (list 23 72 149 34))
;recursive
(define (last-pair-v2 xs)
  (cond ((null? xs) nil)
        ((null? (cdr xs)) (list (car xs)))
        (else (last-pair-v2 (cdr xs)))))

(last-pair-v2 nil)
(last-pair-v2 (list 1))
(last-pair-v2 (list 23 72 149 34))

;iterative
(define (last-pair-v3 xs)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (list (car xs)))))
  (iter xs nil))

(last-pair-v3 nil)
(last-pair-v3 (list 1))
(last-pair-v3 (list 23 72 149 34))

;cps
(define last-pair>>= 
  (lambda (c)
    (lambda (xs)
      (define null?>> (cps null?))
      ((null?>>
       (lambda (b)
         (if b
             (c nil)
             ((null?>> 
              (lambda (b)
                (if b
                    (c (list (car xs)))
                    ((last-pair>>= c) (cdr xs))
                    )
                ))
              (cdr xs))
             )))
       xs)
       )))
                     
   
(define (last-pair-v4 xs) ((last-pair>>= id) xs))
(last-pair-v4 nil)
(last-pair-v4 (list 1))
(last-pair-v4 (list 23 72 149 34))