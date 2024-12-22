#lang racket
(require racket threading racket/hash "../utils.rkt")

(define secret-numbers (~> (get-aoc 2024 22) (string-split "\n") (map string->number _)))

(define (calc-next-secret secret-number)
  (let ([mix (λ (num secret) (bitwise-xor num secret))] [prune (λ (num) (modulo num 16777216))])
    (~> (~> secret-number (* 64) (mix secret-number) prune)
        ((λ (m) (~> m (quotient 32) (mix m) prune)))
        ((λ (d) (~> d (* 2048) (mix d) prune))))))

(define secrets (map (λ~>> (iterate calc-next-secret) (stream-take _ 2001) stream->list) secret-numbers))

;; part 1
(for/sum ([secret secrets]) (last secret))

;; part 2
(define (get-pattern-vals secret-vals)
  (let* ([prices (list->vector (map (curryr modulo 10) secret-vals))]
         [diffs (list->vector (map (λ (p) (- (second p) (first p))) (sliding-window prices 2)))]
         [pattern->price (make-hash)])
    (for ([idx (in-range (- (vector-length diffs) 3))]
          #:do [(define pattern (vector-between diffs idx (+ idx 4))) (define price (vector-ref prices (+ idx 4)))]
          #:unless (hash-has-key? pattern->price pattern))
      (hash-set! pattern->price pattern price))
    pattern->price))

(let ([secret-hshs (map get-pattern-vals secrets)])
  (apply (curryr hash-union! #:combine/key (λ (k v1 v2) (+ v1 v2))) secret-hshs)
  (~> secret-hshs first hash-values (apply max _)))
