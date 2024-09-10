#lang racket
(require racket threading advent-of-code)

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

#|

idea

iterate through, taking the preamble amount prior
then check if two-sum exists for preamble numbers

|#
(define (sliding-window lst n)
  (define (in-list-window l len)
    (make-do-sequence
     (lambda ()
       (values
        (lambda (i) (take (drop l i) len)) ; Extracts a window of length `n`
        add1
        0
        (lambda (i) (<= (+ i len) (length l))) ; Ensures window doesn't exceed the list
        #f
        #f))))
  (for/list ([window (in-list-window lst n)])
    window))

(sliding-window '(1 2 3 4 5 6 7 8 9 10) 5)





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
(for/first ([window (sliding-window XMAS (add1 preamble-length))]
            #:do [(match-define-values (preamble (list target)) (split-at-right window 1))]
            #:when (false? (two-sum preamble target)))
  target)

;; part 2
#|

idea

starting with the part 1 number

oh, we can sort I think
so we find the range of numbers where sum is equal

we want to do a dynamic sliding window
start at bottom and add until we get either:
if the goal, return list
if above the goal, drop the lowest
if below the goal, extend higher
|#


;; part 2 238243506
(define (vector-values-between idx1 idx2 vec)
  (for/list ([idx (in-range idx1 idx2)])
    (vector-ref vec idx)))

(let ([xmas (list->vector XMAS)]
      [goal 1492208709])
  (let loop ([left 0] [right 1])
    (let* ([vals-between (vector-values-between left right xmas)]
           [sum (apply + vals-between)])
      (displayln (format "~a ~a ~a ~a" left right vals-between sum))
      (cond [(equal? sum goal) ((λ (ls)
                                  (apply + (list (apply min ls) (apply max ls))))
                                vals-between)]
            [(< sum goal) (loop left (add1 right))]
            [else (loop (add1 left) right)]))))
