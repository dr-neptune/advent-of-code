#lang racket
(require racket threading advent-of-code srfi/1 algorithms)

(define reports (~> (fetch-aoc-input (find-session) 2024 2 #:cache #t)
                    (string-split "\n")
                    (map
                     (λ~> (string-split " ") (map string->number _)) _)))

;; part 1
(define (small-jumps? ls)
  (all? (map (λ (a b) (let ([diff (abs (- b a))])
                        (and (<= 1 diff) (<= diff 3)))) ls (rest ls))))

(for/sum ([ls reports]
          #:when (and (or (increasing? ls) (increasing? (reverse ls)))
                      (small-jumps? ls)))
  1)

;; part 2
#|

idea
iterate through the lists, and at each step ensure that the conditions are satisfied
keep a count of 1
if you face an error, drop count to 0 and continue
if you face another error, raise an issue

|#

(define (increasing/intermediate? ls)
  (map < ls (rest ls)))

(increasing/intermediate? '(1 2 3 4 4))



(let ([ls '(8 6 4 4 1)])
  (let ([mono-check (increasing/intermediate? ls)])
    (if (< (or (count false? mono-check)
               (count (compose not false?) mono-check)) 2)
        (small-jumps? ())
        #f))
  )


(for/sum ([ls reports]
          #:when (safe-report? ls))
  1)
