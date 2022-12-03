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

;; pt 1
(~>> rucksacks
     (string-split _ "\n")
     (map (λ (v) (~> v
                     string->list
                     (split-at _ (quotient (string-length v) 2))
                     values->list
                     (map list->set _))))
     (map (λ (2ple) (~> (set-intersect (first 2ple) (second 2ple))
                        set-first
                        char->ranking)))
     (foldl + 0))

;; pt 2
(~> rucksacks
    (string-split _ "\n")
    (split-into _ 3)
    (map (compose
          char->ranking
          set-first
          (λ (3tup) (set-intersect (first 3tup) (second 3tup) (third 3tup)))
          (λ (3tup) (map (compose list->set
                                  string->list) 3tup))) _)
    (foldl + 0 _))
