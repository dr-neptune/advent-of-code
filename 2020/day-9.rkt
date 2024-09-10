#lang racket
(require racket threading advent-of-code)

(define XMAS (~> (fetch-aoc-input (find-session) 2020 9)
                 (string-split "\n")
                 (map string->number _)))

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

#|

idea

iterate through, taking the preamble amount prior
then check if two-sum exists for preamble numbers

|#

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))


;; idea
;; match last val to val, first vals to preamble
;; then map across the sliding window with updated two-sum
(match-define [(list* preamble ... val) (sliding-window XMAS 6)])


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


(two-sum '(65 95 102 117 150) 182)
(two-sum '(95 102 117 150 182) 127)
