(require racket threading advent-of-code)

(define expense-reports (~>> (fetch-aoc-input (find-session) 2020 1)
                             string-split
                             (map string->number)))

;; part 1
;; find the 2 entries that sum to 2020 and then multiply them together
(define (balance-report expense-reports num-pairs summation)
  (for/first ([pair (in-combinations expense-reports num-pairs)]
              #:when (equal? (apply + pair) summation))
    (apply * pair)))

(balance-report expense-reports 2 2020)

;; part 2
(balance-report expense-reports 3 2020)
