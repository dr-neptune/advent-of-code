#lang racket
(require racket advent-of-code threading megaparsack megaparsack/text rebellion/base/range racket/match)

(define assignments (string-split (fetch-aoc-input (find-session) 2022 4)))

;; use parser combinators to turn "a-b,c-d" -> '((a b)(c d))
(define (parse-assignment assignment)
  "use parser combinators to parse each assignment"
  (define integer-duo/p (many/p integer/p #:sep (char/p #\-)))
  (define assignment-pairs/p (many/p integer-duo/p #:sep (char/p #\,)))
  (parse-result! (parse-string assignment-pairs/p assignment)))

;;check if one interval is a subset of the other
(define (contains-interval? pair1 pair2)
    (and (<= (first pair1) (first pair2))
         (>= (second pair1) (second pair2))))

(define (overlapping-intervals? assignments)
  (or (apply contains-interval? assignments)
      (apply contains-interval? (reverse assignments))))

(define (disjoint? pair1 pair2)
  (define (in-range? a start end)
    (and (>= a start) (<= a end)))
  (if (contains-interval? pair1 pair2) #f
      (let first-pair ([pair1 pair1])
        (cond [(empty? pair1) #t]
              [(in-range? (first pair1) (first pair2) (second pair2)) #f]
              [else (first-pair (rest pair1))]))))

;; check disjointness over an arbitrary collection of lists
(define (disjoint-lists? lists)
  (let iter-lists ([flist (first lists)]
                   [rest-lists (rest lists)])
    (cond [(empty? rest-lists) #t]
          [(disjoint? flist (first rest-lists))
           (iter-lists (first rest-lists) (rest rest-lists))]
          [else #f])))

;; pt 1
(~>> assignments
     (map (compose overlapping-intervals? parse-assignment))
     (count identity))

;; pt 2
(~>> assignments
     (map (compose not disjoint-lists? parse-assignment))
     (count identity))


;; alternative implementation using range data structures and a macro
(require racket advent-of-code threading megaparsack megaparsack/text rebellion/base/range
         (for-syntax racket/match))

(define assignments (string-split (fetch-aoc-input (find-session) 2022 4)))

;; use parser combinators to turn "a-b,c-d" -> '((a b)(c d))
(define (parse-assignment assignment)
  "use parser combinators to parse each assignment"
  (define integer-duo/p (many/p integer/p #:sep (char/p #\-)))
  (define assignment-pairs/p (many/p integer-duo/p #:sep (char/p #\,)))
  (parse-result! (parse-string assignment-pairs/p assignment)))

;; macro to apply range comparator function and count instances
(define-syntax-rule (count-range-comparator collection fn)
  (~>> collection
       (map (compose
             (λ (pair) (map (λ (p) (apply closed-range p)) pair))
             parse-assignment))
       (map fn)
       (count identity)))

;; pt 1
(count-range-comparator assignments (λ (duo) (or (apply range-encloses? duo)
                                                 (apply range-encloses? (reverse duo)))))

;; pt 2
(count-range-comparator assignments (λ (duo) (apply range-overlaps? duo)))
