#lang racket
(require racket threading advent-of-code)

(define cups
  (~>> "389125467"
       (string-split _ "")
       (filter (compose not (curry equal? "")))
       (map string->number)))

#|

idea
seems like we need a lot of in-cycle

|#

;; simulate a move
(match-let ([(list circle o1 o2 o3 others ...) cups])
  (let ([pick-up (list o1 o2 o3)]
        [destination
         (let ([pool (cons circle others)])
           (let rec ([dest (sub1 circle)])
             (cond [(< (apply min pool) dest) (apply max pool)]
                   [(false? (member dest others)) (rec (sub1 dest))]
                   [else dest])))])
    destination
    (list )))
