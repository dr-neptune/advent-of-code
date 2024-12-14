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

;; with GLPK
(require glpk)

(define (solve-equation-ilp x1 y1 x2 y2 goal-x goal-y [offset 0])
  (define objective `(0 (3 a) (1 b)))   ; 3a + b
  (define direction 'min)
  (define constraints
    `((c1 (,x1 a) (,x2 b))
      (c2 (,y1 a) (,y2 b))))
  (define bounds
    `((c1 ,(+ offset goal-x) ,(+ offset goal-x))
      (c2 ,(+ offset goal-y) ,(+ offset goal-y))
      (a 0 1e13)
      (b 0 1e13)))
  (define integer-vars '(a b))
  (mip-solve objective direction constraints bounds integer-vars))


(define (solve-equations-ilp eqns [offset 0])
  (inexact->exact
   (for/sum ([eqn eqns]
             #:do [(match-define (list x1 y1 x2 y2 gx gy) eqn)]
             [res (solve-equation-ilp x1 y1 x2 y2 gx gy offset)]
             #:when (list? res))
     ;; (println res)
     (match-let ([(list result _) res])
       result))))

;; part 1
(solve-equations-ilp claw-motion)

;; part 2 (breaks racket)
((curryr solve-equations-ilp 10000000000000) claw-motion)
