#lang racket
(require racket racket/hash graph threading "../utils.rkt")

(define grid (~> (get-aoc 2024 16) string->grid/2D))
(define-values (grid-width grid-height start-loc end-loc)
  (values (vector-length (vector-ref grid 0)) (vector-length grid) (car (get-locations/2D grid #\S)) (car (get-locations/2D grid #\E))))

(define (in-bounds? x y) (and (< -1 x grid-width) (< -1 y grid-height) (not (char=? #\# (get-cell/2D grid x y)))))
(define (forward x y d) (let ([offset (list-ref directions/nesw d)]) (list (+ x (first offset)) (+ y (second offset)) d)))

;; Build a weighted graph '(x y direction)
(define edges
  (~>> (for*/list ([x (in-range grid-height)]
                   [y (in-range grid-width)]
                   [dir (in-range 4)]
                   #:when (in-bounds? x y)
                   #:do [(define from-state (list x y dir))])
         (list (list 1000 from-state (list x y (modulo (sub1 dir) 4)))
               (list 1000 from-state (list x y (modulo (add1 dir) 4)))
               (list 1 from-state (apply forward (list x y dir)))))
       (apply append)))

(define-values (dist-hash _) (dijkstra (weighted-graph/directed edges) (list (first start-loc) (second start-loc) 1)))

;; pt 1
(define best-cost (~>> (hash-filter-keys dist-hash (λ (k) (equal? end-loc (take k 2)))) hash-values (apply min)))

;; pt 2
#|
let best-cost be the minimal overall cost from (start-X, start-Y, east (1)) to (end-X, end-Y, any dir):

best-cost = min_{d \in \{0,1,2,3\}} [dist-from-start(end-X, end-Y, d)]

a state (x,y,d) lies on some best path iff
dist-from-start(x,y,d) + dist-to-end(x,t,d) = best-cost

find all cells where this is true and then return the set of them
|#

(define reversed-edges (map (λ (3ple) (list (first 3ple) (third 3ple) (second 3ple))) edges))
(define reversed-graph (weighted-graph/directed reversed-edges))

;; dummy node with cost 0 is a hack to run dijkstra from several source nodes at once
(for ([dir (in-range 4)]) (add-directed-edge! reversed-graph 'dummy (append end-loc (list dir)) 0))

(define-values (dist-hash-rev _) (dijkstra reversed-graph 'dummy))

(~> (for/hash ([(state distF) (in-hash dist-hash)]
               #:do [(define distR (hash-ref dist-hash-rev state +inf.0))]
               #:when (= (+ distF distR) best-cost))
      (values (take state 2) #t))
    hash-count)
