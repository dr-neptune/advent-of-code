#lang racket
(require racket advent-of-code threading)

;; Strategy: Build the lexicographically largest subsequence of length k
;; by repeatedly grabbing the leftmost highest digit that still leaves enough
;; positions for the remaining picks, then recursing on the suffix after it.

;; Helpers
;; Convert a digit character to its numeric value.
(define (char->digit char) (- (char->integer char) (char->integer #\0)))
;; Fold a list of digits into its numeric value (e.g., '(1 2 3) -> 123).
(define (digits->number ds) (for/fold ([n 0]) ([d ds]) (+ (* n 10) d)))
;; Digits to try, in descending order.
(define full-digits '(9 8 7 6 5 4 3 2 1 0))

;; "12345\n67890" -> '((1 2 3 4 5) (6 7 8 9 0))
(define jolts
  (~>> (fetch-aoc-input (find-session) 2025 3)
       (string-split _ "\n")
       (map (compose (curry map char->digit) string->list))))

;; Return #t if there is enough room after idx to place remaining digits.
(define (space? digit-num idx ls-len total-digits)
  (>= (- ls-len (sub1 idx)) (- total-digits digit-num)))

;; Pick the lexicographically largest subsequence of length k from ls.
(define (max-subseq ls k)
  (let loop ([digits full-digits] [digit-num 1] [ls ls])
    (if (> digit-num k) '()
        (match-let ([(list* d more) digits])
          (let* ([len (length ls)] [idx (index-of ls d)]
                 [space-available? (and idx (space? digit-num idx len k))])
            (if space-available?
                (cons d (loop full-digits (add1 digit-num) (list-tail ls (add1 idx))))
                (loop more digit-num ls)))))))

;; part 1 + 2
(for/list ([k (list 2 12)])
  (for/sum ([battery jolts]) (~> battery (max-subseq k) digits->number)))
