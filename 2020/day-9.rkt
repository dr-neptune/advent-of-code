#lang racket
(require racket threading advent-of-code
         rebellion/streaming/transducer
         rebellion/collection/list)

(define XMAS (~> (fetch-aoc-input (find-session) 2020 9)
                 (string-split "\n")
                 (map string->number _)))

(define preamble-length 25)

(define XMAS
  (~> "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"
      (string-split "\n")
      (map string->number _)))

(define preamble-length 5)

(define (sliding-window ls n [into into-list])
  (transduce ls (windowing n) #:into into))

(define (vector-values-between idx1 idx2 vec)
  (for/list ([idx (in-range idx1 idx2)])
    (vector-ref vec idx)))

(define (two-sum nums target)
  (define (iter nums)
    (if (or (null? nums) (null? (rest nums)))
        #f
        (let ([fs (first nums)]
              [ls (last nums)])
          (match (- (+ fs ls) target)
            [0 (list fs ls)]
            [(? negative?) (iter (rest nums))]
            [(? positive?) (iter (drop-right nums 1))]))))
  (define get-nums (iter (sort nums <)))
  (if get-nums
      (map (λ (v) (index-of nums v)) get-nums)
      #f))

;; part 1: 1492208709
(define p1-answer
  (for/first ([window (sliding-window XMAS (add1 preamble-length))]
              #:do [(match-define-values (preamble (list target)) (split-at-right window 1))]
              #:when (false? (two-sum preamble target)))
    target))

;; part 2 238243506
(let ([xmas (list->vector XMAS)]
      [goal p1-answer])
  (let loop ([left 0] [right 1])
    (let* ([vals-between (vector-values-between left right xmas)]
           [sum (apply + vals-between)])
      (cond [(equal? sum goal) ((λ (ls)
                                  (apply + (list (apply min ls) (apply max ls))))
                                vals-between)]
            [(< sum goal) (loop left (add1 right))]
            [else (loop (add1 left) right)]))))
