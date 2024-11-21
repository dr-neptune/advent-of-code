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


(define re-pattern #px"^(\\w+): capacity (\\d+), durability (\\d+), flavor (\\d+), texture (\\d+), calories (\\d+)")

(regexp-match re-pattern "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")

(define ingredients
  (~>
   "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
   ;; (fetch-aoc-input (find-session) 2015 15)
   (string-split "\n")
   (map
    (λ (s) (match-let ([(list _ ingredient capacity durability flavor texture calories)
                        (regexp-match re-pattern s)])
             (list ingredient capacity durability flavor texture calories)))
    _)))
