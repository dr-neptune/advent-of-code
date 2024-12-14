#lang racket
(require racket math/matrix math/array threading "../utils.rkt")

(define claw-motion
  (~> (get-aoc 2024 13)
      (string-split "\n\n")
      (map (λ~>> (regexp-match* #px"[+-]?\\d+") (map string->number)) _)))

(define (solve-equation x1 y1 x2 y2 goal-x goal-y [add 0])
  (define M (matrix [[x1 x2] [y1 y2]]))
  (define B (col-matrix [(+ add goal-x) (+ add goal-y)]))
  (matrix-solve M B))

;; part 1
(for/sum ([eqn claw-motion]
           #:do [(match-define (list a b) (array->list (apply solve-equation eqn)))]
           #:when (andmap integer? (list a b)))
  (+ (* 3 a) b))

;; part 2
(for/sum ([eqn claw-motion]
          #:do [(match-define (list a b) (array->list (apply (curryr solve-equation 10000000000000) eqn)))]
           #:when (andmap integer? (list a b)))
  (+ (* 3 a) b))
