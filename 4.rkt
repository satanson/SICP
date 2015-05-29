#lang r5rs
(define (make-set) '())
(define (exist? s x)
  (cond ((null? s) #f)
        ((eq? (car s) x) #t)
        (else (exist? (cdr s) x))))

(define (join-set! s x)
  (if (exist? s x)
      s
      (begin (set! s (cons x s))
             s)))

(define S (make-set))
(join-set! S 1)
(join-set! S 2)
(join-set! S 3)
(display S)
