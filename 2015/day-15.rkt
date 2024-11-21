#lang racket
(require racket threading advent-of-code)

#|

idea

I think this is the knapsack problem with 4 constraints
maybe not, just a combinatorial optimization problem

idea
given n inputs, make a linear function for each attribute
then apply and take the max
with 4 ingredients this seems doable by brute force
with many more, it would take some cleverness

|#


(define re-pattern #px"^(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)")

(define ingredients
  (~>
;;    "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
;; Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
   (fetch-aoc-input (find-session) 2015 15)
   (string-split "\n")
   (map
    (λ (s) (match-let ([(list _ ingredient capacity durability flavor texture calories)
                        (regexp-match re-pattern s)])
             (cons ingredient (map string->number (list capacity durability flavor texture calories)))))
    _)))


;; essentially we are taking the dot product of 2 lists
;; splits and (capacity / durability / flavor / texture)
;; for each ingredient

(define (dprod a b)
  (let ([total (apply + (map * a b))])
    (if (negative? total)
        0
        total)))

(define (groupwise . args) args)

(define (calculate-amounts . splits)
  (define (calculate-value capacities durabilities flavors textures calories)
    (map (curry dprod splits) (list capacities durabilities flavors textures calories)))
  calculate-value)

(define (find-quadruplets target-sum)
  (for*/list ([a (in-range 1 (- target-sum 3))]
              [b (in-range (add1 a) (- target-sum 2))]
              [c (in-range (add1 b) (- target-sum 1))]
              [d (in-range (add1 c) target-sum)]
              #:when (= (+ a b c d) target-sum))
    (list a b c d)))

(define quadruplets (find-quadruplets 100))

;; (define (find-2-tuples target-sum)
;;   (for*/list ([a (in-range 1 (- target-sum 3))]
;;               [b (in-range (add1 a) (- target-sum 2))]
;;               #:when (= (+ a b) target-sum))
;;     (list a b)))

;; (define 2-tuples (find-2-tuples 100))

(define (calculate-values break-points)
  (apply (apply calculate-amounts break-points)
         (apply (curry map groupwise) (map rest ingredients))))

(apply max
       (map (compose
             (curry apply *)
             (curryr drop-right 1)
             calculate-values)
            quadruplets))

;; wrong answer, wah wah
