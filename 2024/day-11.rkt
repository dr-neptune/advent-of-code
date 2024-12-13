#lang racket
(require racket threading memo)

(define stones '(6 11 33023 4134 564 0 8922422 688775))

(define (num-digits n) (add1 (exact-floor (log n 10))))  ;; d = floor(log_b(n)) + 1

(define (split-integer n)
  (define-values (l r) (quotient/remainder n (expt 10 (quotient (num-digits n) 2))))
  (list l r))

(define/memoize (apply-rules stone blinks)
  (cond [(zero? blinks) 1]
        [(zero? stone) (apply-rules 1 (sub1 blinks))]
        [else
         (let ([digits (num-digits stone)])
           (if (odd? digits)
               (apply-rules (* 2024 stone) (sub1 blinks))
               (~>> stone split-integer (map (curryr apply-rules (sub1 blinks))) (apply +))))]))

(for/sum ([stone stones]) (apply-rules stone 75))  ;; or 25 for pt 1
