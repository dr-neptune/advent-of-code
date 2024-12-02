#lang racket
(require racket
         threading
         advent-of-code
         algorithms)

(define reports (~> (fetch-aoc-input (find-session) 2024 2 #:cache #t)
                    (string-split "\n")
                    (map
                     (λ~> (string-split " ") (map string->number _)) _)))

(define (is-increasing? ls)
  (all? (adjacent-map (λ (a b) (and (< a b) (<= 1 (- b a) 3))) ls)))

(define (is-decreasing? ls)
  (all? (adjacent-map (λ (a b) (and (> a b) (<= 1 (- a b) 3))) ls)))

(define (is-safe ls) (or (is-increasing? ls) (is-decreasing? ls)))

;; part 1
(for/sum ([ls reports]
          #:when (is-safe ls))
  1)

;; part 2
(define (is-safe-with-dampener ls)
  (or (is-safe ls)
      (ormap (λ (i)
               (is-safe (append (take ls i) (drop ls (+ i 1)))))
             (build-list (length ls) values))))

(count identity (map is-safe-with-dampener reports))
