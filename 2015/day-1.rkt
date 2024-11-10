#lang racket
(require racket threading advent-of-code)

(define parens (~> (fetch-aoc-input (find-session) 2015 1)
                   (string-split "")))

;; part 1
(for/sum ([paren parens])
  (match paren
    ["(" 1]
    [")" -1]
    [_ 0]))


;; part 2
(for/fold ([fnum 0] [idx 0]
           #:result (sub1 idx))
          ([paren parens]  #:break (negative? fnum))
  (match paren
    ["(" (values (add1 fnum) (add1 idx))]
    [")" (values (sub1 fnum) (add1 idx))]
    [_ (values fnum (add1 idx))]))
