#lang racket
(require racket threading advent-of-code (only-in srfi/1 unfold-right))

(define (get-aoc year day)
  (fetch-aoc-input (find-session) year day #:cache #t))

(define (string->grid/2D str)
  (~> str
      (string-split "\n")
      (map (λ~> string->list list->vector) _)
      list->vector))

(define (get-cell/2D vec x y) (vector-ref (vector-ref vec y) x))

(define directions+diag
  '((0 -1)    ;; North
    (1 -1)    ;; North-East
    (1 0)     ;; East
    (1 1)     ;; South-East
    (0 1)     ;; South
    (-1 1)    ;; South-West
    (-1 0)    ;; West
    (-1 -1))) ;; North-West

(define directions/nesw
  '((0 -1)    ;; North
    (1 0)     ;; East
    (0 1)     ;; South
    (-1 0)))  ;; West

(define (get-locations/2D puzzle char)
  (for*/list ([row-idx (in-range (vector-length puzzle))]
              [col-idx (in-range (vector-length (vector-ref puzzle 0)))]
              #:when (equal? (get-cell/2D puzzle col-idx row-idx) char))
    (list col-idx row-idx)))

(define (get-nearby/2D vec x y #:dirs [dirs directions/nesw] #:bound? [bound? #t])
  (define vec-width (sub1 (vector-length (vector-ref vec 0))))
  (define vec-height (sub1 (vector-length vec)))

  (define (safe? nx ny)
    (if bound?
        (and (<= 0 nx vec-width) (<= 0 ny vec-height))
        #t))

  (filter (curry apply safe?)
       (map (λ~> (map + _ (list x y)))
            dirs)))

(define (flatten-depth lst [depth 1])
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(define flatten/1 flatten-depth)
(define flatten/2 (curryr flatten-depth 2))

(define (int->digit-list int)
  (unfold-right zero? (curryr remainder 10) (curryr quotient 10) int))

(define (char->number char)
  (- (char->integer char) (char->integer #\0)))

(define (dot-product ls1 ls2)
  (for/sum ([ele1 ls1] [ele2 ls2])
    (* ele1 ele2)))

(provide (all-defined-out))
