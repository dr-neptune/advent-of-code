#lang racket
(require racket)
(require threading)

(require "./read-aoc-input.rkt")

(define calories (read-aoc-input "1"))

(define (calorie-+ elf)
  (~>> elf
     (map string->number)
     (foldl + 0)))

(~> calories
    (string-split "\n\n")
    (map (lambda v (~> v first (string-split "\n") calorie-+)) _)
    (sort <)
    (take-right 3)
    (foldl + 0 _))
