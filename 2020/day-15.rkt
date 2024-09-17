#lang racket
(require racket)

(define starting-numbers '(0 1 5 10 3 12 19))
(define nth-num 2020)

(define (index-of/2 ls v)
  (let loop ([ls ls] [i 0] [found-indices '()])
    (cond
      [(or (null? ls) (>= (length found-indices) 2)) found-indices]
      [(equal? (car ls) v) (loop (cdr ls) (add1 i) (cons i found-indices))]
      [else (loop (cdr ls) (add1 i) found-indices)])))

(define (idx-of/2/abs-diff ls v)
  (apply (compose abs +) (index-of/2 ls v)))

;; part 1
(let ([seen (list->mutable-set starting-numbers)])
  (for/fold ([nums (reverse starting-numbers)]
             #:result (cadr nums))
            ([idx (in-inclusive-range 0 (- nth-num (length starting-numbers)))])
  (let ([recent (car nums)])
    (cond [(false? (set-member? seen recent)) (begin (set-add! seen recent) (cons 0 nums))]
          [else (cons (idx-of/2/abs-diff nums recent) nums)]))))

;; part 2
(define nth-num 30000000)

(let ([seen (make-hash '([0 . 1] [1 . 2] [5 . 3] [10 . 4] [3 . 5] [12 . 6] [19 . 7]))])
  (for/fold ([prev-number (last starting-numbers)])
            ([idx (in-inclusive-range (add1 (length starting-numbers)) nth-num)])
    (let ([prev-turn (hash-ref seen prev-number #f)])
      (begin
        (hash-set! seen prev-number (sub1 idx))
        (match prev-turn
          [#f 0]
          [_ (- (sub1 idx) prev-turn)])))))
