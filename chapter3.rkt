#lang racket/load
(require scheme/mpair)
(load "util.rkt")
;Exercise 3.1.
(define (make-accumulator n)
  (lambda(m)(begin (set! n (+ n m)) n)))
(define A (make-accumulator 5))
(A 10)
;15
(A 10)
;25

;Exercise 3.2.

(define (make-monitored f)
  (let ((count 0))
    (lambda (x . y)
      (cond ((eq? x 'how-many-calls?)
             count)
            ((eq? x 'reset-count)
             (begin (set! count 0) 0))
            (else
             (begin (set! count (+ count 1))
                    (apply f (cons x y))))))))

(define s (make-monitored sqrt))

(s 100)
;10

(s 'how-many-calls?)
;1
(define (square x) (* x x))
(map (compose s square) (range 1 10))
(s 'how-many-calls?)


;Exercise 3.3.

(define (make-account balance password)
  (define (withdraw n)
    (if (> n balance)
        "Insufficient funds"
        (begin (set! balance (- balance n))
               balance)))
  (define (deposit n)
    (begin (set! balance (+ balance n))
           balance))
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'dispose) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))) 
        (lambda x "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
(println ((acc 'secret-password 'withdraw) 40))
;60

(println ((acc 'some-other-password 'deposit) 50))
;"Incorrect password"

(define (rand-range lo hi)
  (+ lo (* (random) (- hi lo))))
;(for-each println (map (lambda(x)(rand-range 1 10)) (range 1 10)));ok

(define (monte-carlo trials cesaro-test)
  (define (iter n succ)
    (if (eq? n 0)
        (/ succ trials)
        (if (cesaro-test)
            (iter (- n 1) (+ succ 1))
            (iter (- n 1) succ))))
  (iter trials 0))

(define (rand-int) (random 4294967087))
(define (gcd a b)
  (define (iter x y)
    (if (eq? y 0)
        x
        (let ((r (modulo x y)))
          (iter y r))))
  (iter a b))
                  
(define (pi n)
  (sqrt (/ 6 (monte-carlo n (lambda()(eq? 1 (gcd (rand-int) (rand-int))))))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((rect-area (* (- x2 x1) (- y2 y1)))
        (cesaro-test (lambda()(P (rand-range x1 x2) (rand-range y1 y2)))))
    (* rect-area (monte-carlo trials cesaro-test))))

(define (pi2 trials)
  (estimate-integral (lambda(x y)(< (+ (square x) (square y)) 1)) -1 1 -1 1 trials))

;(println (pi2 100000))

;Exercise 3.17.

(define (make-set . xs)
  (define set (apply mlist xs))
  (define (contains? e)
    (if (null? (filter (lambda (x)(eq? x e)) (mlist->list set)))
        #f
        #t))
  (define (add! e)
    (if (contains? e) '() (set! set (mcons e set))))
  (define (->string)
    (display (mlist->list set)))
  (lambda (m)
    (cond ((eq? m 'contains?) contains?)
          ((eq? m 'add!) add!)
          ((eq? m '->string) ->string)
          (else (lambda xs (error "Not Implemented Method!" m))))))

(define S (make-set 1 2 3))
((S 'remove!) 1)

(define (make-fifo . xs)
  (define fifo (apply mlist xs))
  (define (empty?)(null? fifo))
  (define (push! e) (set! fifo (apply mlist (append (mlist->list fifo) (list e)))))
  (define (pop!)
    (if (empty?)
        (error "Empty FIFO" "can't pop any thing from it")
        (let ((e (mcar fifo)))
          (begin (set! fifo (mcdr fifo)) e))))
  (lambda (m)
    (cond ((eq? m 'empty?) empty?)
          ((eq? m 'push!) push!)
          ((eq? m 'pop!) pop!)
          (else (error "Not Supported Mothed" m)))))


(define q (make-fifo))
((q 'empty?))
((q 'push!) 1)
((q 'push!) 2)

(define (count-pairs a)
  (define q (make-fifo))
  (define s (make-set))
  (define (iter q s n)
    (if ((q 'empty?))
        n
        (let* ((e ((q 'pop!)))
              (x (car e))
              (y (cdr e)))
          (begin 
            (if (and (pair? x) (not ((s 'contains?) x)))
                (begin
                  ((q 'push!) x)
                  ((s 'add!) x))
                '())
            (if (and (pair? y) (not ((s 'contains?) y)))
                (begin
                  ((q 'push!) y)
                  ((s 'add!) y))
                '())            
            (iter q s (+ n 1))))))
  (if (pair? a)
      (begin
        ((q 'push!) a)
        ((s 'add!) a)
        (iter q s 0))
      0))

(println (count-pairs (list 1 2 3)))

(define x (list 1 2))
(define y (cons x x ))


(println (count-pairs y))
(define x1 (list 1))
(define x2 (cons x1 x1))
(define x3 (cons x2 x2))
(println (count-pairs x3))


(define (count-pairs1 x)
  (if (not (pair? x))
      0
      (+ (count-pairs1 (car x))
         (count-pairs1 (cdr x))
         1)))
(println (count-pairs1 y))
(println (count-pairs1 x3))