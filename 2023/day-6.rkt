#lang racket
(require racket advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

(define boat-races (string-split (fetch-aoc-input (find-session) 2023 6) "\n"))

(define boat-races/p
  (let ([space+ (many/p space/p)])
    (do (many/p (char-between/p #\A #\z))
        space+
      (string/p ":")
      space+
      [boat-ints <- (many/p integer/p #:sep (many/p space/p))]
      (pure boat-ints))))

(define time-dists
  (let ([parsed (map (compose parse-result! (curry parse-string boat-races/p)) boat-races)])
    (map cons (first parsed) (second parsed))))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

;; part 1
(define (winning-ways time-dists)
  (let* ([move-fn (λ (v t) (* v (- t v)))]
         [check-fn (λ (t dist)
                     (stream->list
                      (stream-filter (curry < dist)
                                     (stream-map (curryr move-fn t) (in-range 0 t)))))])
    (apply * (map (λ (td) (length (check-fn (car td) (cdr td)))) time-dists))))

;; part 2
(define concat-time-dists
  (let ([concat-digit-list (λ (digit-list) (digit-list->int (flatten (map int->digit-list digit-list))))])
    (cons (concat-digit-list (map car time-dists))
          (concat-digit-list (map cdr time-dists)))))

(winning-ways (list concat-time-dists))
