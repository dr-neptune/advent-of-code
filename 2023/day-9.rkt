#lang racket
(require racket (only-in srfi/1 map) advent-of-code)

(define oasis
  (map (compose (curry map string->number) string-split)
       (string-split (fetch-aoc-input (find-session) 2023 9) "\n")))

(define (next-level ls)
  (map (λ (a b) (- b a)) ls (rest ls)))

(define (all-equal? ls [char '()])
  (andmap (λ (a) (equal? a (car ls))) ls))

(define (predict-next digit-ls)
  (let loop ([ls digit-ls])
    (cond [(all-equal? ls) (first ls)]
          [else (+ (last ls) (loop (next-level ls)))])))

;; part 1
(apply + (map predict-next oasis))

;; part 2
(apply + (map (compose predict-next reverse) oasis))
