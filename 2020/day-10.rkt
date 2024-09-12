#lang racket
(require racket threading advent-of-code
         (only-in srfi/1 map))

(define adapter-joltage (~> (fetch-aoc-input (find-session) 2020 10)
                            (string-split "\n")
                            (map string->number _)))

#|

idea
dynamic programming? sort then see if we can reach an end state?
sort, then start from the back. We know the jumps will be either 1 or 3 jolts at a time
the end state is 0. We want to use all adapters
|#

(define adapter-joltage
  (~> "16
10
15
5
1
11
7
19
6
12
4"
      (string-split "\n")
      (map string->number _)))





(define adapter-joltage
  (~>
   "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"
   (string-split "\n")
   (map string->number _)))



(define jolts
  (cons (+ 3 (apply max adapter-joltage))
        (append (sort adapter-joltage >) '(0))))

;; part 1
(let* ([counts (map - jolts (rest jolts))])
  (apply * (list (count (curry equal? 3) counts)
                 (count (curry equal? 1) counts))))

;; part 2
#|

idea
iterate through from the beginning, sorted
for each step, find all steps that are possible next and recurse there
at the end, return a 1 if it is able to reach the end state
sum them up

|#

(define (vector-values-between idx1 idx2 vec)
  (for/list ([idx (in-range idx1 idx2)])
    (vector-ref vec idx)))

(let ([jolts (list->vector (map cons (range 0 (length jolts)) (reverse jolts)))])
  (apply +
         (flatten
   (let rec ([idx 0])
    (match idx
      [(== (vector-length jolts)) 1]
      [_
       (let* ([curr-val (cdr (vector-ref jolts idx))]
              [possibilities
               (filter
                (λ (v) (<= (- (cdr v) curr-val) 3))
                (vector-values-between (add1 idx) (min (vector-length jolts) (+ idx 4)) jolts))])
         ;; (displayln (format "idx: ~a curr: ~a possible: ~a" idx curr-val possibilities))
         (if (empty? possibilities)
             1
             (map (compose rec car) possibilities)))])))))

;; with memoization
(let* ([jolts (list->vector (map cons (range 0 (length jolts)) (reverse jolts)))]
       [memo (make-hash)]) ; Create a hash table for memoization
  (define (rec idx)
    (cond
      [(hash-has-key? memo idx) ; Check if the result is already memoized
       (hash-ref memo idx)]     ; Return the memoized result
      [(= idx (vector-length jolts)) ; Base case: reached the end of the vector
       1]
      [else
       (let* ([curr-val (cdr (vector-ref jolts idx))]
              [possibilities
               (filter
                (λ (v) (<= (- (cdr v) curr-val) 3))
                (vector-values-between (add1 idx) (min (vector-length jolts) (+ idx 4)) jolts))]
              [result
               (if (empty? possibilities)
                   1
                   (apply + (map (compose rec car) possibilities)))])
         (hash-set! memo idx result) ; Store the result in the memo table
         result)])) ; Return the computed result
  (rec 0)) ; Start recursion from idx 0
