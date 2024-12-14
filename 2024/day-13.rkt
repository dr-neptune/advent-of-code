#lang racket
(require racket math/matrix math/array threading "../utils.rkt")

(define claw-motion
  (~> (get-aoc 2024 13) (string-split "\n\n")
      (map (λ~>> (regexp-match* #px"[+-]?\\d+") (map string->number)) _)))

(define (solve-equation x1 y1 x2 y2 goal-x goal-y [add 0])
  (define M (matrix [[x1 x2] [y1 y2]]))
  (define B (col-matrix [(+ add goal-x) (+ add goal-y)]))
  (matrix-solve M B))

(define (solve-equations eqns [solver-fn (curry apply solve-equation)])
  (for/sum ([eqn eqns]
            #:do [(match-define (list a b) (array->list (solver-fn eqn)))]
            #:when (andmap integer? (list a b)))
    (+ (* 3 a) b)))

;; part 1
(solve-equations claw-motion)

;; part 2
(solve-equations claw-motion (curry apply (curryr solve-equation 10000000000000)))
