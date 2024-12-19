#lang racket
(require racket threading "../utils.rkt")

(match-define (list towels designs)
  (~> (get-aoc 2024 19) (string-split "\n\n")
      ((λ (pair) (list (string-split (first pair) ", ")
                       (string-split (second pair) "\n"))))))

(define (feasible? design)
  (let* ([n (string-length design)] [dp (vector-set/copy (make-vector (add1 n) 0) 0 1)])
    (for* ([idx (in-range n)] [pattern towels]
           #:do [(define plen (string-length pattern)) (define vec-offset (+ idx plen))]
           #:when (and (<= vec-offset n) (string=? (substring design idx vec-offset) pattern) (vector-ref dp idx)))
      (vector-set! dp vec-offset (+ (vector-ref dp vec-offset) (vector-ref dp idx))))
    (vector-ref dp n)))

;; pt 1
(~>> (map feasible? designs) (filter (compose not zero?)) length)

;; pt 2
(apply + (map feasible? designs))
