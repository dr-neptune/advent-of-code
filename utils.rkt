#lang racket
(require racket threading advent-of-code)

(define (get-aoc year day)
  (fetch-aoc-input (find-session) year day #:cache #t))

(define (string->grid/2D str)
  (~> str
      (string-split "\n")
      (map (λ~> string->list list->vector) _)
      list->vector))

(define (get-cell/2D vec x y) (vector-ref (vector-ref vec y) x))

(define (get-locations/2D puzzle char)
  (for*/list ([row-idx (in-range (vector-length puzzle))]
              [col-idx (in-range (vector-length (vector-ref puzzle 0)))]
              #:when (char=? (get-cell/2D puzzle col-idx row-idx) char))
    (list col-idx row-idx)))

(define (flatten-depth lst [depth 1])
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(define flatten/1 flatten-depth)
(define flatten/2 (curryr flatten-depth 2))

(provide string->grid/2D get-cell/2D get-locations/2D flatten-depth flatten/1 flatten/2 get-aoc)
