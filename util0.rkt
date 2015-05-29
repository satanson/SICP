;#lang racket
(define (foldr f z xs)
  (if (null? xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (car xs)) (cdr xs))))

(define (filter p xs)
  (define (f x xs)(if (p x) (cons x xs) xs))
  (foldr f '() xs))

(define-syntax comment
  (syntax-rules ()
               ((comment x ...) (display ""))))
(comment
(define (length xs)
  (define (f x y) (+ 1 y))
  (foldr f 0 xs))
)

;range-impl::Integer->Integer->Integer->[Integer]
;(range-impl start stop step)
;(range-impl 1 10 2): [1,3,5,7,9]
;note: stop excluding

(define (range-impl start stop step)
  (if (>= start stop)
      '()
      (cons start (range-impl (+ start step) stop step))))
(define-syntax mrange
  (syntax-rules()
    ((mrange stop)(range-impl 0 stop 1))
    ((mrange start stop)(range-impl start stop 1))
    ((mrange start stop step)(range-impl start stop step))))
;take::Integer->[a]->[a]
;(take n xs)
;(take 2 '(1 2 3 4 5 6)):(1 2)
(define (take n xs)(car (split n xs)))
(define (drop n xs) (cadr (split n xs)))
(define (split n xs)
  (define (iter n xs1 xs2)
    (if (or (<= n 0) (null? xs2))
        (list xs1 xs2)
        (iter (- n 1)  (append xs1 (list (car xs2))) (cdr xs2))))
  (iter n '() xs))

(define (range . xs)
  (case (length xs)
    ((1) (range-impl 0 (car xs) 1))
    ((2) (range-impl (car xs) (cadr xs) 1))
    (else (range-impl (car xs) (cadr xs) (caddr xs)))))

;flatmap::(a->[b])->[a]->[b]
;(flatmap f xs)

(define (flatmap f xs)
  (foldr append '() (map f xs)))

;(flatmap range (range 1 6))

(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (x y)(f y x)))

(define (nary xs n ) (eq? (length xs) n))
(define (unary? xs) (nary xs 1))
(define (binary? xs) (nary xs 2))
(define (ternary? xs) (nary xs 3))
(define (quarternary? xs) (nary xs 4))

(define (square x) (* x x))

(define (error msg arg)
  (begin (display (string-append "ERROR: " msg))
         (display ": ")
         (display arg)
         (newline)))
            
(define (println . args)
  (define (print_iter xs)
    (if (null? xs)
        (newline)
        (begin (display (car xs))
               (display " ")
               (print_iter (cdr xs)))))
  (print_iter args))