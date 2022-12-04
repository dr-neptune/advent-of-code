#lang racket
(require racket advent-of-code threading
         (only-in br/list values->list))

(define rucksacks (fetch-aoc-input (find-session) 2022 3))

(define (char->ranking character)
  (cond [(char-upper-case? character)
         (- (char->integer character) 38)]
        [else (- (char->integer character) 96)]))

(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))

(define (set-intersection* set-ls) (apply set-intersect set-ls))

;; pt 1
(~>> rucksacks
     (string-split _ "\n")
     (map (λ (ruck) (~>> ruck
                     string->list
                     (split-at _ (quotient (string-length ruck) 2))
                     values->list
                     (map list->set))))
     (map (compose char->ranking set-first set-intersection*))
     (foldl + 0))

;; pt 2
(~>> rucksacks
    (string-split _ "\n")
    (split-into _ 3)
    (map (compose
          char->ranking
          set-first
          set-intersection*
          (λ (3ruck) (map (compose list->set string->list) 3ruck))))
    (foldl + 0))
