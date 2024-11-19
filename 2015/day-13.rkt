#lang racket
(require racket threading advent-of-code racket/hash
         rebellion/streaming/transducer
         rebellion/collection/list)

(define (sliding-window ls n [into into-list])
  (transduce ls (windowing n) #:into into))

(define guests
  (~>
   (fetch-aoc-input (find-session) 2015 13)
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



;; part 1
(apply max
       (for/list ([perm (in-permutations guest-names)])
         (let ([circle (append perm (list (first perm)))])
           (for/sum ([pair (sliding-window circle 2)])
             (+ (hash-ref guest-map pair)
                (hash-ref guest-map (reverse pair)))))))


;; part 2
(define guest-names (append guest-names (list "You")))

(define guests
  (append
   (flatten (for/list ([guest guest-names])
              (list (format "~a would gain 0 happiness units by sitting next to You." guest)
                    (format "You would gain 0 happiness units by sitting next to ~a." guest))))
   guests))


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

(apply max (for/list ([perm (in-permutations guest-names)])
  (let ([circle (append perm (list (first perm)))])
    (for/sum ([pair (sliding-window circle 2)])
      (+ (hash-ref guest-map pair)
         (hash-ref guest-map (reverse pair)))))))
