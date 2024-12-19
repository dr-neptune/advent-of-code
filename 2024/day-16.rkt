#lang racket
(require racket graph threading "../utils.rkt")

#|

idea

make a graph
rm blocks
find all paths through the graph
apply any direction changes to the paths
calculate points

|#

(define reindeer-map
  (~> "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"
      string->grid/2D))

(define-values (grid-width grid-height) (values (vector-length (vector-ref reindeer-map 0))
                                                (vector-length reindeer-map)))


(define (get-nearby/2D x y #:dirs [dirs directions/nesw] #:bound? [bound? #t])
  (define (safe? nx ny) (if bound? (and (<= 0 nx grid-width) (<= 0 ny grid-height)) #t))
  (filter (curry apply safe?) (map (λ~> (map + _ (list x y))) dirs)))

(define reindeer-graph
  (let ([graph-base (for*/list ([x (in-inclusive-range 0 grid-width)]
                                [y (in-inclusive-range 0 grid-height)])
                      (list x y))])
    (unweighted-graph/adj (for/list ([node graph-base]) (cons node (apply get-nearby/2D node))))))

(for ([wall (in-list (get-locations/2D reindeer-map #\#))])
  (remove-vertex! reindeer-graph wall))

(get-locations/2D reindeer-map #\S)
(get-locations/2D reindeer-map #\E)

(~> reindeer-graph (dijkstra (first (get-locations/2D reindeer-map #\S))))
(~> reindeer-graph (bfs (first (get-locations/2D reindeer-map #\S))))
