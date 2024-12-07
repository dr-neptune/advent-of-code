#lang racket
(require racket threading advent-of-code memo)

(define equations
  (map (λ (line)
         (match (string-split line ": ")
           [(list sum-str summands-str)
            (list (string->number sum-str)
                  (map string->number (string-split summands-str " ")))]))
       (string-split (fetch-aoc-input (find-session) 2024 7 #:cache #t) "\n")))

(define (interleave ls1 ls2)
  (match ls1
    ['() ls2]
    [_ (cons (car ls1) (interleave ls2 (cdr ls1)))]))

(define/memoize (n-cart-prod elements size) (apply cartesian-product (make-list size elements)))

(define/memoize (r->l-eval ls)
  (match ls
    [(list a) a]
    [(list a b c _ ...)
     (r->l-eval (cons (b a c) (cdddr ls)))]))

(define (calculate-calibrations equations operators)
  (let ([get-op-match (λ (sum summands)
           (for/first ([operator-combination (n-cart-prod operators (sub1 (length summands)))]
                       #:do [(define eqn-eval (r->l-eval (interleave summands operator-combination)))]
                       #:when (equal? eqn-eval sum))
             eqn-eval))])
    (for/sum ([eq equations]
              #:do [(match-define (list sum summands) eq)
                    (define op-match (get-op-match sum summands))]
              #:when (not (false? op-match)))
      op-match)))

;; pt 1
(calculate-calibrations equations (list + *))

;; pt 2
(define (conc a b) (~>> (list a b) (map number->string) (apply string-append) string->number))

(calculate-calibrations equations (list conc + *))
