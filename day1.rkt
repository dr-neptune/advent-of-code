#lang racket
(require racket threading advent-of-code)

(define calories (fetch-aoc-input (find-session) 2022 1))

(define (calorie-+ elf)
  (~>> elf
     (map string->number)
     (foldl + 0)))

(~> calories
    (string-split "\n\n")
    (map (lambda v (~> v first (string-split "\n") calorie-+)) _)
    (sort >)
    (take 3)
    (foldl + 0 _))
