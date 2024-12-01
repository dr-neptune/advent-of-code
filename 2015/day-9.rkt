#lang racket
(require racket threading advent-of-code
         rebellion/streaming/transducer
         rebellion/collection/list)

#|

idea

this is the traveling salesman problem

idea

make a hash that shows distances between each city, e.g. (london, dublin) . 464 / (dublin, london) . 464

since santa is flying, he can go to each in any order
so we have (num cities)! permutations which we can calculate
then we can sort

|#

(define distances
  (~>
   (fetch-aoc-input (find-session) 2015 9)
   ;; "London to Dublin = 464
;; London to Belfast = 518
;; Dublin to Belfast = 141"
   (string-split "\n")))

(define dists
  (let ([hsh (make-hash)])
    (for ([line distances])
      (match-let* ([(list locs dist) (string-split line " = ")]
                   [(list a b) (string-split locs " to ")]
                   [dist (string->number dist)])
        (hash-set! hsh (list a b) dist)
        (hash-set! hsh (list b a) dist)))
    hsh))

(define cities ((compose remove-duplicates flatten hash-keys) dists))

(define (sliding-window ls n [into into-list])
  (transduce ls (windowing n) #:into into))

;; part 1
(apply min
       (map (compose
             (curry apply +)
             (λ (p) (map (curry hash-ref dists) p))
             (curryr sliding-window 2))
            (permutations cities)))


;; part 2
(apply max
       (map (compose
             (curry apply +)
             (λ (p) (map (curry hash-ref dists) p))
             (curryr sliding-window 2))
            (permutations cities)))
