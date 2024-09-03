#lang racket
(require racket threading advent-of-code)

(define customs (~> (fetch-aoc-input (find-session) 2020 6) (string-split "\n\n")))

;; part 1
(~>> customs
     (map (λ (sheet) (~> sheet
                         (string-replace "\n" "")
                         string->list
                         remove-duplicates
                         length)))
     (apply +))

;; part 2
(~>> customs
     (map (λ (sheet) (~>> sheet
                          string-split
                          (map (λ (subls)(list->set (string->list subls))))
                          (apply set-intersect)
                          set-count)))
     (apply +))
