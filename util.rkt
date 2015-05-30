(define println (lambda (x) (display x) (newline)))
(define print (lambda (x) (display "")))
(define range (lambda x (range-args x)))

(define (range-args args)
  (cond
    ((= (length args) 1) (range-impl 0 (car args) 1))
    ((= (length args) 2) (range-impl (car args) (cadr args) 1))
    ((= (length args) 3) (range-impl (car args) (cadr args) (caddr args)))
    )
  )

(define (range-impl b e s)
  (cond
    ((< b e) (cons b (range-impl (+ b s) e s)))
    ('())
  )
)

(define (foldl f z xs)
  (if (null? xs)
      z
      (let (
            (head (car xs))
            (tail (cdr xs))
            )
        (foldl f (f z head) tail)
        )
      )
  )

(define (foldr f z xs)
  (if (null? xs)
      z
      (let (
            (head (car xs))
            (tail (cdr xs))
            )
        (f head (foldr f z tail))
        )
      )
  )
(define nil '())
(define reduce foldr)
(define foldRight foldr)
(define foldLeft foldl)
(define reduceRight foldr)
(define reduceLeft foldl)

(define (filter f xs) 
  (define (g y ys)
    (if (f y)
        (cons y ys)
        ys
        )
    )
  (foldr g '() xs)
  )

(define grep filter)


(define (++ . xs) 
  (define (++2 ys zs)(foldr cons zs ys))
  (foldr ++2 '() xs)
  )

(define (qsort xs)
  (if (null? xs)
      xs
      (let* (
            (pivot (car xs))
            (xs_ (cdr xs))
            (xs1 (filter (lambda (x) (< x pivot)) xs_ ))
            (xs2 (filter (lambda (x) (>= x pivot)) xs_ ))
            )
        (++ (qsort xs1) (list pivot) (qsort xs2))
        )
      )
  )
;cps
(define (cps f)
  (lambda (cont)
    (lambda args (cont (apply f args)))))

;identity function
(define (I x) x)
(define id I)
;
(define (flip binary-f)(lambda (x y) (binary-f y x)))
;
(define (flatten xs)
  (cond ((null? xs) xs)
        ((not (pair? (car xs))) (cons (car xs) (flatten (cdr xs))))
        (else (append (flatten (car xs)) (flatten (cdr xs))))))

(define (<> path)
  (define (read-iter port ch chs)
    (if (eof-object? ch)
        chs
        (read-iter port (read-char port) (cons ch chs))
        )
    )
  
  (call-with-input-file 
      path 
    (lambda (port) (list->string (reverse (read-iter port (read-char port) '())))
    )
    )
  )
 
(define (datumize path)
  (define (read-iter port t ts)
    (if (eof-object? t)
        ts
        (read-iter port (read port) (cons t ts))
        )
    )
  
  (call-with-input-file 
      path 
    (lambda (port) (reverse (read-iter port (read port) '()))
    )
    )
  )
(define (fib)
  (let (
        (fn_2 0)
        (fn_1 1)
        )
    (lambda ()
      (let (
            (fn_1_ fn_1)
            )
         
        (set! fn_1 (+ fn_1 fn_2))
        (set! fn_2 fn_1_)
        fn_1
        )
      )
    )
  )
;(define f (fib))
;(map (lambda(x) (f)) (range 1 10))

(define (id x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combiner result (term a)))))
  (accumulate-iter a null-value))

(define (sum-range term a next b) (accumulate + 0 term a next b))
(define (prod term a next b) (accumulate * 1 term a next b))

(define (filter-next filter? next n)
    (define (filter-next-iter  m)
      (if (filter? m)
          m
          (filter-next-iter (next m))))
    (filter-next-iter (next n)))
  

(define (filter-accumulate filter? combiner null-value term a next b)
  (define (filter-next n)
    (define (filter-next-iter  m)
      (if (filter? m)
          m
          (filter-next-iter (next m))))
    (filter-next-iter (next n)))
  (define (adjust a)(if (filter? a) a (filter-next a)))
  
  (accumulate combiner null-value term (adjust a) filter-next b))


(define (even-sum-range a b) (filter-accumulate even? + 0 id a inc b))
(define (odd-sum-range a b) (filter-accumulate odd? + 0 id a inc b))

;(+ (even-sum-range 1 10) (odd-sum 1 10))

;(define s (<> "util.rkt"))
;(display s)
(define (sum xs) (foldr + 0 xs))
(define (n! n) (foldr * 1 (range 1 n)))
(define (mean xs) (/ (sum xs) (length xs)))

(define (search-zero-point f neg-pt pos-pt n)
  (define close-enough? (precision? n))
  (let (
        (mid-pt (mean (list neg-pt pos-pt)))) 
    (if (close-enough? neg-pt pos-pt)
        mid-pt
        (let (
              (test-value (f mid-pt)))
          (cond ((positive? test-value)
                 (search-zero-point f neg-pt mid-pt n))
                ((negative? test-value)
                 (search-zero-point f mid-pt pos-pt n))
                (else mid-pt))))))

(define (precision? n)
  (lambda (a b) (< (abs (- a b)) (/ 1 (expt 10 n)))))

(define close-enough? (precision? 6))

;(+ 0.0 (search-zero-point square -1 2 6))
;(+ 0.0 (search-zero-point (lambda (x)(- (cube x) 10)) 2 3 6))

(define (half-interval-method f a b)
  (let(
        (ya (f a))
        (yb (f b)))
    (cond
      ((and (negative? ya) (positive? yb))(search-zero-point f a b 6))
      ((and (negative? yb) (positive? ya))(search-zero-point f b a 6))
      (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point-verbose f first-guess verbose)
  (define tolerance 0.000001)
  (define (close-enough? a b)(< (abs (- a b)) tolerance))
  (define (iter guess)
    (let (
          (next-guess (f guess)))
      (verbose guess)
      (if (close-enough? guess next-guess)
          (begin
            (verbose next-guess)
            next-guess)
          (iter next-guess))))
  (iter first-guess))




(define (fixed-point f first-guess)(fixed-point-verbose f first-guess print0))
;Newton's Method
(define dx 0.0000001)
(define (deriv g)
  (lambda (x)
    (let* (
           (y+dy (g (+ x dx)))
           (y (g x))
           (dy (- y+dy y)))
      (/ dy dx))))

(define (newton-transform f)
  (lambda (x)
    (let* (
           (df (deriv f))
           (fx (f x))
           (dfx (df x)))
      (- x (/ fx dfx)))))

(define (newton-method f x)(fixed-point (newton-transform f) 1.0)) 
(define (sqrt3 x)
  (newton-method (lambda (y) (- (square y) x))
                  1.0)) 

(define (sqrt2 x)
  (fixed-point (lambda (y) (mean (list y (/ x y))))
               1.0))
 
(define (average-damp f)
  (lambda (x)
    (mean (list x (f x)))))

(define (fixed-point-of-transform  f transform x)
  (fixed-point (transform f) x))

(define (sqrt4 x)(fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))
(define (sqrt5 x)(fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
;(sqrt2 2)
;(sqrt3 2)
;(sqrt4 2)
;(sqrt5 2)
(define-syntax comment
  (syntax-rules ()
    ((comment) (display "") )
    ((comment e ...) (display ""))))

(define (compose f g)(lambda(x) (f (g x))))
(define (repeated f n)
  (define (repeated-iter n g)
    (if (zero? n)
        g
        (repeated-iter (- n 1) (compose f g))))
  (repeated-iter n id))

(define (iterative-improve good-enough? improve-guess)
  (lambda (init-guess)
    (define (iter guess)
      (let (
            (next-guess (improve-guess guess)))
        (if (good-enough? next-guess guess)
            next-guess
            (iter next-guess))))
    (iter init-guess)))
