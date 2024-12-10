#lang racket
(require racket threading "../utils.rkt")

(define topographic-map
  (~>> (get-aoc 2024 10) string->grid/2D (vector-map (λ~>> (vector-map char->number)))))

(define (nearby-values+coords x y)
  (let ([nearby (get-nearby/2D topographic-map x y)])
    (map (λ (coord) (list (get-cell/2D topographic-map (car coord) (cadr coord)) coord)) nearby)))

;; pt 1
(for/sum ([trailhead (get-locations/2D topographic-map 0)])
  (set-count
   (let loop ([curr-coord trailhead] [elevation 0])
     (if (equal? 9 elevation)
         (set curr-coord)
         (let* ([nearby (nearby-values+coords (first curr-coord) (second curr-coord))]
                [next-steps (map second (filter (λ (pair) (equal? (add1 elevation) (first pair))) nearby))])
           (foldl (λ (nc acc) (set-union acc (loop nc (add1 elevation)))) (set) next-steps))))))

;; pt 2
(for/sum ([trailhead (get-locations/2D topographic-map 0)])
  (let loop ([curr-coord trailhead] [elevation 0])
    (if (equal? 9 elevation)
        1
        (let* ([nearby (nearby-values+coords (first curr-coord) (second curr-coord))]
               [next-steps (map second (filter (λ (pair) (equal? (add1 elevation) (first pair))) nearby))])
          (if (empty? next-steps)
              0
              (apply + (map (λ (next-step) (loop next-step (add1 elevation))) next-steps)))))))
