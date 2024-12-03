#lang racket
(require racket threading advent-of-code)

(define (parse-muls str)
  (define rxpat #rx"mul\\(([0-9]+),([0-9]+)\\)")
  (~>> str
       (regexp-match* rxpat #:match-select cdr)
       (map (λ~>> (map string->number)))))

(define memory (fetch-aoc-input (find-session) 2024 3 #:cache #t))

(define (dprod ls) (for/sum ([ele ls]) (apply * ele)))

;; part 1
(~> memory parse-muls dprod)

;; part 2
(define do_pat #rx"do\\(\\).*?don't\\(\\)")

(let* ([drop-matches (regexp-match* do_pat memory)])
  (~>> drop-matches (append-map parse-muls) dprod))
