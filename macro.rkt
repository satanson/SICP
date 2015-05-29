(define-syntax seq
(syntax-rules ()
((seq exp ...)
((lambda ()  exp ...)))))

(define-syntax ||
  (syntax-rules ()
    ((||) #f)
    ((|| e) e)
    ((|| e1 e2 ...) (if e1 e1 (|| e2 ...)))))

(define-syntax &&
  (syntax-rules ()
    ((&&) #t)
    ((&& e) e)
    ((&& e1 e2 ...) (if e1 (&& e2 ...) e1))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...) body1 body2 ...)))tag) val ...))
    )
  )
(let fib ((n 10)) 
  (cond 
    ((= n 0) 0) 
    ((= n 1) 1)
    ((= n 2) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))
    )
  )
(define-syntax let
  (syntax-rules (in)
    ((let in e ...)((lambda () e ... ))) 
    ((let (a1 v1) ... in e ... )
     ((lambda (a1 ...) e ...) v1 ...))
    ))