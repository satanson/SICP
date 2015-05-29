(load "util.rkt")

(define (prime? n)
  (call-with-current-continuation
   (lambda(return)
     (define (iter k)
       (if (> (* k k) n)
           #t
           (if (zero? (modulo n k))
               (return #f)
               (iter (+ k 1)))))
     (iter 2))))

(define (prime1? n)
  (call-with-current-continuation
   (lambda(return)
     (do ((k 2 (+ 1 k)))
       ((> (* k k) n) #t)
       (if (zero? (modulo n k)) (return #f))))))
;(filter prime1? (range 2 100))

(define (prime-sum? pair)
  (prime? (apply + pair)))

(define (make-pair-sum pair)
  (append pair (list (apply + pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)(map (lambda(j)(list j i)) (range 1 i)))
   (range 1 (+ n 1))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


;(prime-sum-pairs 6)
(define (remove x xs)(filter (lambda (xx)(not (= x xx))) xs))
;(remove 1 (range 1 10))
(define (powerset xs)
  (if (null? xs)
      (list '())
      (flatmap 
       (lambda(x)(map (lambda(s)(cons x s)) (powerset (remove x xs)))) 
       xs)))

;(powerset '(1 2 3))


(define (compose f g)
  (lambda x (f (apply g x))))

(define (unique-triples n)
  (foldr append '() 
         (foldr append '() 
                (map (lambda(i)
                       (map(lambda(j)
                             (map (lambda (k) (list k j i))
                                  (range 1 j)))
                           (range 1 i))) 
                     (range 1 (+ n 1))))))

(define (triple-sum s n)
  (filter (lambda(t)(= s (apply + t))) (unique-triples n)))

