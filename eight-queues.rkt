(load "util.rkt")
(define empty-board '())
(define (safe? k positions)
  (if (= k 1) 
      #t
      (let* ((init-and-last (split (- k 1) positions))
             (rest-of-queens (car init-and-last))
             (kth-queen (cadr init-and-last))
             (conflict-queens (filter (lambda (queen)(or 
                                                      (eq? (car queen) (car kth-queen))
                                                      (eq? (cadr queen) (cadr kth-queen))
                                                      (eq? 0 (+ (- (car queen)(car kth-queen))
                                                                (- (cadr queen) (cadr kth-queen))))
                                                      (eq? 0 (- (- (car queen)(car kth-queen))
                                                                (- (cadr queen) (cadr kth-queen))))))
                                      rest-of-queens)))
        (null? conflict-queens))))

(define (adjoin-position new-row k rest-of-queens) (append rest-of-queens (list k new-row)))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 (+ 1 board-size))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)