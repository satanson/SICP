(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p xs)
  (define (f x xs) (cons (p x) xs))
  (accumulate f '() xs))
(define (length xs)
  (define (f x sum) (+ 1 sum))
  (accumulate f 0 xs))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
