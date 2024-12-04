#lang racket
(require racket threading advent-of-code)

(define puzzle
  (~> (fetch-aoc-input (find-session) 2024 4 #:cache #t)
      (string-split "\n")
      (map (λ~> string->list list->vector) _)
      list->vector))

(define directions
  '((0 -1)    ;; North
    (1 -1)    ;; North-East
    (1 0)     ;; East
    (1 1)     ;; South-East
    (0 1)     ;; South
    (-1 1)    ;; South-West
    (-1 0)    ;; West
    (-1 -1))) ;; North-West

(define (get-xy vec x y) (vector-ref (vector-ref vec y) x))

(define (get-nearby vec x y [limit 4] [directions directions])
  (define (safe-get nx ny)
    (if (and (>= nx 0) (< nx (vector-length (vector-ref vec 0)))
             (>= ny 0) (< ny (vector-length vec)))
        (get-xy vec nx ny)
        #f))

  (define (collect-direction dx dy)
    (define positions
      (for/list ([i (in-range 0 limit)])
        (define nx (+ x (* i dx)))
        (define ny (+ y (* i dy)))
        (safe-get nx ny)))
    (filter (λ (c) c) positions))

  (map (λ (dir) (apply collect-direction dir)) directions))

(define (get-locations puzzle char)
  (for*/list ([row-idx (in-range (vector-length puzzle))]
              [col-idx (in-range (vector-length (vector-ref puzzle 0)))]
              #:when (char=? (get-xy puzzle col-idx row-idx) char))
    (list col-idx row-idx)))

;; part 1
;; find X locations
;; return diagonals from X locs and set length of search to 4
;; match against configurations that are XMAS
(for/sum ([coord (get-locations puzzle #\X)]
          #:do [(define found (apply (curry get-nearby puzzle) coord))]
          #:when (not (empty? found)))
  (count (λ~> (equal? '(#\X #\M #\A #\S))) found))

;; part 2
;; find A locations
;; return diagonals from A locs and set length of search to 1
;; match against configurations that work

(define diagonals
  '((1 -1)    ;; North-East
    (1 1)     ;; South-East
    (-1 1)    ;; South-West
    (-1 -1))) ;; North-West

(define good-outcomes
  '(((#\A #\M) (#\A #\S) (#\A #\S) (#\A #\M))   ;; top ms
    ((#\A #\S) (#\A #\M) (#\A #\M) (#\A #\S))   ;; bottom ms
    ((#\A #\S) (#\A #\S) (#\A #\M) (#\A #\M))   ;; mixed ms 1
    ((#\A #\M) (#\A #\M) (#\A #\S) (#\A #\S)))) ;; mixed ms 2

(for/sum ([coord (get-locations puzzle #\A)]
          #:do [(define found (get-nearby puzzle (first coord) (second coord) 2 diagonals))]
          #:when (and (andmap (λ~> length (= 2)) found)
                      (member found good-outcomes)))
  1)
