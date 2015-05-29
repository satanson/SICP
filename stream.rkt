#lang racket
(require r5rs)
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
(define pow2
  (let f ()
    (s-cons 1 (s-scale (f) 2))))

;(s-for-each println (s-drop (s-take pow2 64) 0))

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

(define (pow n)
  (let stream ()
    (s-cons 1 (s-scale (stream) n))))
(define ones2 (pow -1))
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
                   (pow (/ pi 4))))
            20))
;0.9999991904960968
)