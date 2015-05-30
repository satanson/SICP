#lang racket
(require r5rs)
(require racket/list)
(define-syntax s-cons
  (syntax-rules()
    ((s-cons a b) (cons a (delay b)))))

(define-syntax comment
  (syntax-rules()
    ((comment a ...) 'commented)))


(define s-nil '())
(define s-null? null?)
(define (s-car s) (car s))
(define (s-cdr s) (force (cdr s)))
(define (s-for-each proc s)
  (if (s-null? s)
      'done
      (begin
        (proc (s-car s))
        (s-for-each proc (s-cdr s)))))

(define (println x)
  (display x)
  (newline))

(define (print x)
  (display x)
  (display " "))

(define (s-take s n)
  (if (= n 0)
      s-nil
      (s-cons (s-car s) (s-take (s-cdr s) (- n 1)))))

(define (s-drop s n)
  (if (= n 0)
      s
      (s-drop (s-cdr s) (- n 1))))

(define integers
  (let f ((n 0)) (s-cons n (f (+ n 1)))))

(define (s-range low high)
  (s-take (s-drop integers low) (+ high 1 (- low))))

(define ones 
  (let f () (s-cons 1 (f))))

(define (s-foldr f z s)
  (if (s-null? s)
      z
      (f (s-car s) (s-foldr f z (s-cdr s)))))
(define (s-filter p s)
  (if (s-null? s)
      s-nil
      (if (p (s-car s))
          (s-cons (s-car s) (s-filter p (s-cdr s)))
          (s-filter p (s-cdr s)))))


(define (s-map t . ss)
  (if (s-null? (car ss))
      s-nil
      (s-cons 
       (apply t (map s-car ss)) 
       (apply s-map t (map s-cdr ss)))))
(define (s-nth s n)
  (if (= n 0)
      (s-car s)
      (s-nth (s-cdr s) (- n 1))))

(define (fibgen a b)
  (s-cons a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
;(s-for-each println (s-take fibs 10))
;(s-for-each print (s-take ones 10))



;(let f ((a 10)) (if (= a 0) 1 (* a (f (- a 1)))))

(define a
  ((lambda()
     (define b '())
     (set! b (lambda(x) x))
     (b 10))))

;(s-for-each print (s-range 1 10))
;(s-for-each println (s-take fibs 10))
;(s-foldr + 0 (s-take fibs 10))
;(s-for-each println (s-filter even? (s-take fibs 10)))
;(s-for-each println (s-filter odd? (s-take fibs 10)))

;(s-for-each println (s-take integers 10))
;(s-for-each println (s-range 0 10))

(define (show x)
  (println x)
  x)
;(define x (s-map show (s-range 0 10)))
;(s-nth x 5)
;(s-nth x 7)

(define (divisible?  x y) (= (remainder x y) 0))
(define (sieve stream)
  (s-cons
   (begin
     ;(print "s-car=")
     ;(println (s-car stream))
     (s-car stream)
     )
   (sieve (s-filter
           (lambda (x)
             ;(print "x=")
             ;(println x)
             (not (divisible? x (s-car stream))))
           (begin
             ;(print "s-cdr stream first 5 elements:")
             ;(s-for-each print (s-take (s-cdr stream) 5))
             ;(println "")
             (s-cdr stream))))))

(define primes (sieve (s-drop integers 2)))
;(s-for-each println (s-take primes 100))
;(s-drop integers 3)
;(s-take (s-filter even? (s-drop integers 3)) 10)
;(s-for-each println (s-take (s-drop integers 2) 10))

;(s-for-each println (s-take (s-drop integers 2) 1000))
;(s-for-each println (s-take (s-map + integers (s-drop integers 1)) 10))
(define (s-add sa sb) (s-map + sa sb))
(define integers1
  (let f ()
    (s-cons 1 (s-add ones (f)))))

;(s-for-each println (s-take integers1 10))

(define foobar
  (let f ()
    (s-cons 1 (s-map + (f) (f)))))

;(s-for-each println (s-drop (s-take foobar 20) 0))

(define (s-scale s a) (s-map (lambda(x)(* a x)) s))
(define geometric-series2
  (let f ()
    (s-cons 1 (s-scale (f) 2))))

;(s-for-each println (s-drop (s-take geometric-series2 64) 0))

(define (s-mul sa sb) (s-map * sa sb))
(define factorials
  (let f ()
    (s-cons 1 (s-mul (s-drop integers 1) (f)))))

;(s-for-each println (s-drop (s-take factorials 10) 0))

(define (partial-sums s)
  (let stream ()
    (s-cons (s-car s) (s-add (stream) (s-cdr s)))))
;(s-for-each println (s-take (partial-sums integers) 10))


(define (s-merge s1 s2)
  (cond ((s-null? s1) s2)
        ((s-null? s2) s1)
        (else
         (let ((s1car (s-car s1))
               (s2car (s-car s2)))
           (cond ((< s1car s2car)
                  (s-cons s1car (s-merge (s-cdr s1) s2)))
                 ((> s1car s2car)
                  (s-cons s2car (s-merge s1 (s-cdr s2))))
                 (else
                  (s-cons s1car
                          (s-merge (s-cdr s1)
                                   (s-cdr s2)))))))))


(define S
  (let stream ()
    (s-cons 1 (s-merge (s-scale (stream) 2)
                       (s-merge (s-scale (stream) 3)
                                (s-scale (stream) 5))))))

;(s-for-each println (s-take S 20))

(define (expand num den radix)
  (s-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (s-println s)
  (print "[ ")
  (s-for-each print s)
  (println " ]"))
;(s-println (s-take (expand 3 8 10) 20))

;(s-println (s-take (expand 1 3 10) 20))

(define (integrate-series s)
  (let stream ((n 1)
               (xs s))
    (s-cons (/ (s-car xs) n) (stream (+ n 1) (s-cdr xs)))))

(define exp-series
  (s-cons 1 (integrate-series exp-series)))

;(s-println (s-take exp-series 20))
;(s-println (s-map (lambda(x)(+ x 0.0))(s-take (partial-sums exp-series) 20)))


(define cosine-series
  (s-cons 1 (s-scale (integrate-series sine-series) -1)))

(define sine-series
  (s-cons 0 (integrate-series cosine-series)))

;(s-println (s-take cosine-series 20))
;(s-println (s-take sine-series 20))

(define (mul-series s1 s2)
  (s-cons (* (s-car s1) (s-car s2)) 
          (s-add (s-scale (s-cdr s1) (s-car s2))
                 (s-add (s-scale (s-cdr s2) (s-car s1))
                        (s-cons 0 (mul-series (s-cdr s1) (s-cdr s2)))))))

;(s-println (s-take
;            (s-add (mul-series sine-series sine-series)
;                   (mul-series cosine-series cosine-series))
;            20))

(define (geometric-series n)
  (let stream ()
    (s-cons 1 (s-scale (stream) n))))
(define ones2 (geometric-series -1))
;(s-println (s-take ones2 10))
;(s-println (s-take (mul-series ones ones2) 10))

(define (ones-zeros n)
  (let stream ((m 0))
    (s-cons (if (= 0 (modulo m n)) 1 0) (stream (+ m 1)))))
(comment  
 (s-println (s-take
             (mul-series 
              (ones-zeros 1)
              (mul-series
               (ones-zeros 5)
               (mul-series
                (ones-zeros 10)
                (mul-series
                 (ones-zeros 25)
                 (mul-series
                  (ones-zeros 50)
                  (ones-zeros 100))))))
             1000))
 
 )

(define (invert-unit-series s)
  (let stream ()
    (s-cons 1 (s-scale (mul-series (stream) (s-cdr s)) -1))))

;(s-println (s-take (invert-unit-series (ones-zeros 1)) 5))
(define (div-series s1 s2)
  (if (= 0 (s-car s2))
      (error "constant-term of denominator is zero!" "")
      (let ((invert-const-term (/ 1 (s-car s2))))
        (mul-series
         (s-scale (invert-unit-series (s-scale s2 invert-const-term)) 
                  invert-const-term)
         s1))))

(define tangent-series (div-series sine-series cosine-series))
(comment
 (define pi 3.14159265358979)
 (s-println (s-take 
             (partial-sums 
              (s-mul tangent-series
                     (geometric-series (/ pi 4))))
             20))
 ;0.9999991904960968
 )
(define (average . xs)
  (/ (apply + xs) (length xs)))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-series a)
  (let stream ()
    (s-cons 1 (s-map (lambda(x)(sqrt-improve  x a))(stream)))))
;(s-println (s-take (sqrt-series 2.0) 20))
;(s-println (s-take (sqrt-series 2.0) 20))
;(comment
(define pi-series
  (let ((odds-series (s-filter odd? integers))
        (geometric-series-of-minus-one (geometric-series -1)))
    (s-scale
     (s-map (lambda(x) (/ 1.0 x)) 
            (s-mul odds-series geometric-series-of-minus-one))
     4)))
;)
;(s-println (s-take (partial-sums pi-series) 1000)) 
(define square (lambda(x) (* x x)))
(define (euler-transform s)
  (let ((s0 (s-nth s 0))           ; Sn-1
        (s1 (s-nth s 1))           ; Sn
        (s2 (s-nth s 2)))          ; Sn+1
    (s-cons (- s2 (/ (square (- s2 s1))
                     (+ s0 (* -2 s1) s2)))
            (euler-transform (s-cdr s)))))

;(s-println (s-take (euler-transform (partial-sums pi-series)) 1000))
(define (make-tableau transform s)
  (s-cons s
          (make-tableau transform
                        (transform s))))
(define (accelerated-sequence transform s)
  (s-map s-car
         (make-tableau transform s)))
(comment
 (s-println (s-take (accelerated-sequence euler-transform
                                          (partial-sums pi-series)) 8))
 )

(define (s-limit s boundary)
  (let ((a (s-car s))
        (b (s-car (s-cdr s))))
    (if (< (abs (- a b)) boundary)
        b
        (s-limit (s-cdr s) boundary))))
;(s-limit (sqrt-series 2.0) 0.0001)

(define ln2-series
  (let ((geometric-series-of-minus-one (geometric-series -1))
        (pos-integers (s-drop integers 1)))
    (s-map (lambda(x) (/ 1.0 x)) 
           (s-mul pos-integers geometric-series-of-minus-one))))

(define ln2 (partial-sums ln2-series))
;(comment
;(s-println (s-take (accelerated-sequence euler-transform ln2) 10))
;)

(define (pairs s t)
  (s-cons
   (list (s-car s) (s-car t))
   (interleave
    (s-map (lambda (x) (list (s-car s) x))
           (s-cdr t))
    (pairs (s-cdr s) (s-cdr t)))))

(define (interleave s1 s2)
  (if (s-null? s1)
      s2
      (s-cons (s-car s1)
              (interleave s2 (s-cdr s1)))))

(define (s-take-until p s)
  (if (or (s-null? s) (p (s-car s)))
      s-nil
      (s-cons (s-car s) (s-take-until p (s-cdr s)))))
(define (s-drop-until p s)
  (if (or (s-null? s) (p (s-car s)))
      s
      (s-drop-until p (s-cdr s))))
(define (s-length s)
  (s-foldr + 0 (s-map (lambda(_) 1) s)))
(define pos-integers (s-drop integers 1))
(define in-pairs (pairs pos-integers pos-integers))
(define (print-pairs-until pair)
  ;(s-println (s-take-until (lambda(x)(equal? x pair))in-pairs))
  (print "length=")
  (println (s-length (s-take-until (lambda(x)(equal? x pair))in-pairs))))
(comment
 (print-pairs-until '(1 100))
 (print-pairs-until '(99 100))
 (print-pairs-until '(100 100))
 )

(define (pairs2 s t)
  (if (or (s-null? s) (s-null? t))
      s-nil
      (interleave
       (s-map (lambda (x) (list (s-car s) x))
              t)
       (pairs2 (s-cdr s) (s-cdr t)))))
;(comment
(define pairs3
  (let ((integer-0-9 (s-take integers 10)))
    (pairs2 integer-0-9 integer-0-9)))
;)
;(println (s-length pairs3))
(define (flatten0 orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))

(define (triples S T U)
  (s-map (lambda(x)(flatten0 x))
         (pairs S (pairs T U))))
(define (pythagorean? x y z) (= (+ (square x) (square y)) (square z)))
(define pythagorean-triples
  (s-filter (lambda(xyz)(apply pythagorean? xyz))
            (triples pos-integers pos-integers pos-integers)))
;(s-println (s-take pythagorean-triples 2))


;(flatten0 '((1) 2 3))