#lang racket
(require racket
         advent-of-code
         (prefix-in reb: rebellion/base/range)
         (prefix-in opt: rebellion/base/option)
         (prefix-in rs:  rebellion/collection/range-set))

;; Puzzle input
(define ingredient-ids (fetch-aoc-input (find-session) 2025 5))

;; Split input into range section and ingredient section.
(match-define (list range-section ingredient-section) (string-split ingredient-ids "\n\n"))

;; Parse the id ranges into closed range objects
(define id-ranges
  (for/list ([m (in-list (regexp-match* #px"(\\d+)-(\\d+)" range-section #:match-select cdr))])
    (apply reb:closed-range (map string->number m))))

(define available-ingredients
  (map string->number (string-split ingredient-section "\n" #:repeat? #t)))

;; Merge overlaps/touching ranges so range-set can be built safely.
(define (merge-bounds bounds)
  (for/fold ([acc '()]) ([b bounds])
    (match-let ([(list lo hi) b])
      (match acc
        ['() (list b)]
        [(cons (list mlo mhi) rest)
         (if (<= lo (add1 mhi))      ;; overlap or touch
             (cons (list mlo (max mhi hi)) rest)
             (cons b acc))]))))

(define merged-ranges
  (apply rs:range-set
         (let ([sorted (sort (map (λ (r)
                                    (list (reb:range-lower-endpoint r)
                                          (reb:range-upper-endpoint r)))
                                  id-ranges)
                             < #:key first)])
           (map (λ (b) (apply reb:closed-range b))
                (merge-bounds sorted)))))

;; part 1: count ingredient ids that fall in any merged range.
(define (fresh? id)
  (opt:present?
   (rs:range-set-range-containing-or-absent merged-ranges id)))

(for/sum ([id (in-list available-ingredients)]
          #:when (fresh? id))
  1)

;; part 2: total coverage across all merged ranges.
(for/sum ([r (rs:in-range-set merged-ranges)])
  (add1 (- (reb:range-upper-endpoint r)
           (reb:range-lower-endpoint r))))
