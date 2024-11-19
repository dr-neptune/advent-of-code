#lang racket
(require racket threading advent-of-code racket/hash
         rebellion/streaming/transducer
         rebellion/collection/list)

(define guests
  (~>
   (fetch-aoc-input (find-session) 2015 13)
;;    "Alice would gain 54 happiness units by sitting next to Bob.
;; Alice would lose 79 happiness units by sitting next to Carol.
;; Alice would lose 2 happiness units by sitting next to David.
;; Bob would gain 83 happiness units by sitting next to Alice.
;; Bob would lose 7 happiness units by sitting next to Carol.
;; Bob would lose 63 happiness units by sitting next to David.
;; Carol would lose 62 happiness units by sitting next to Alice.
;; Carol would gain 60 happiness units by sitting next to Bob.
;; Carol would gain 55 happiness units by sitting next to David.
;; David would gain 46 happiness units by sitting next to Alice.
;; David would lose 7 happiness units by sitting next to Bob.
;; David would gain 41 happiness units by sitting next to Carol."
   (string-split "\n")))

(define guest-names '("Alice" "Bob" "Carol" "David" "Eric" "Frank" "George" "Mallory"))

(define guest-map
  (let ([re-pattern #px"^(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)\\.$"])
    (make-hash
     (map (λ (g) (match-let* ([(list _ a parity happiness b) (regexp-match re-pattern g)]
                              [happiness (string->number happiness)])
                   (cons (list a b)
                         (match parity
                           ["gain" happiness]
                           ["lose" (* -1 happiness)]))))
          guests))))



(define (sliding-window ls n [into into-list])
  (transduce ls (windowing n) #:into into))

;; part 1
(apply max (for/list ([perm (in-permutations guest-names)])
  (let ([circle (append perm (list (first perm)))])
    (for/sum ([pair (sliding-window circle 2)])
      (+ (hash-ref guest-map pair)
         (hash-ref guest-map (reverse pair)))
      ))))


;; part 2
(define guests
  (append
   (flatten (for/list ([guest guest-names])
              (list (format "~a would gain 0 happiness units by sitting next to You" guest)
                    (format "You would gain 0 happiness units by sitting next to ~a" guest))))
   guests))


(define guest-names '("Alice" "Bob" "Carol" "David" "Eric" "Frank" "George" "Mallory" "You"))

(define guest-map
  (let ([re-pattern #px"^(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+)\\.$"])
    (make-hash
     (map (λ (g) (match-let* ([(list _ a parity happiness b) (regexp-match re-pattern g)]
                              [happiness (string->number happiness)])
                   (cons (list a b)
                         (match parity
                           ["gain" happiness]
                           ["lose" (* -1 happiness)]))))
          guests))))

(define (sliding-window ls n [into into-list])
  (transduce ls (windowing n) #:into into))
