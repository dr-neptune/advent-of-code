#lang racket
(require racket threading advent-of-code)

(match-define (list rules updates)
  (~> (fetch-aoc-input (find-session) 2024 5 #:cache #t)
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
