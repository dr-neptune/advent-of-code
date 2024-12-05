#lang racket
(require racket threading advent-of-code)

(match-define (list rules updates)
  (~> (fetch-aoc-input (find-session) 2024 5 #:cache #t)
;;       "47|53
;; 97|13
;; 97|61
;; 97|47
;; 75|29
;; 61|13
;; 75|53
;; 29|13
;; 97|29
;; 53|29
;; 61|53
;; 97|53
;; 61|29
;; 47|13
;; 75|47
;; 97|75
;; 47|61
;; 75|61
;; 47|29
;; 75|13
;; 53|13

;; 75,47,61,53,29
;; 97,61,53,29,13
;; 75,29,13
;; 75,97,47,61,53
;; 61,13,29
;; 97,13,75,29,47"
      (string-split "\n\n")
      (map (curryr string-split "\n") _)
      ((λ (ls-pair)
         (let ([split->num (λ (delim) (λ~> (string-split delim) (map string->number _)))])
           (list (map (split->num "|") (first ls-pair))
                 (map (split->num ",") (second ls-pair))))))))

(define (check-rule num1 num2)
  (for/or ([rule rules])
    (equal? (list num1 num2) rule)))

(define (midpoint ls) (list-ref ls (quotient (length ls) 2)))

;; pt 1
(for/sum ([update updates]
          #:do [(define sorted-update (sort update check-rule))]
          #:when (equal? sorted-update update))
  (midpoint update))

;; pt 2
(for/sum ([update updates]
          #:do [(define sorted-update (sort update check-rule))]
          #:when (not (equal? sorted-update update)))
  (midpoint sorted-update))

;; with topological sort. Works on example. Cycle detected in full input
(require mischief/sort mischief/dict)

(define neighbors (for/hash ([node (group-by car rules)])
                    (values (caar node)
                            (map second node))))

(define tsort-order
  (for/hash ([idx (in-naturals)]
             [ord (reverse (topological-sort
                            (hash-keys neighbors)
                            (dict->procedure #:failure (const empty) neighbors)))])
   (values ord idx)))

(define (t-cmp a b)
  (let ([a-idx (hash-ref tsort-order a)]
        [b-idx (hash-ref tsort-order b)])
    (if (< a-idx b-idx) #t #f)))

;; pt 1
(for/sum ([update updates]
          #:do [(define sorted-update (sort update t-cmp))]
          #:when (equal? sorted-update update))
  (midpoint update))

;; pt 2
(for/sum ([update updates]
          #:do [(define sorted-update (sort update t-cmp))]
          #:when (not (equal? sorted-update update)))
  (midpoint sorted-update))
