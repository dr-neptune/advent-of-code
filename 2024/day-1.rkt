#lang racket
(require racket threading advent-of-code)

(define location-ids (~> (fetch-aoc-input (find-session) 2024 1)
                         (string-split "\n")
                         (map (λ~>
                               (string-split "   ")
                               (map string->number _))
                              _)))

(match-define (list left-list right-list)
  (call-with-values (λ ()
                      (for/lists (p1 p2)
                                 ([pair location-ids])
                        (apply values pair))) list))

;; part 1
(for/sum ([ll (in-list (sort left-list <))]
          [rl (in-list (sort right-list <))])
  (abs (- ll rl)))

;; part 2
(for/sum ([ele (in-list left-list)])
  (* ele (count (curry equal? ele) right-list)))
