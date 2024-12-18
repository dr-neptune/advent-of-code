#lang racket
(require racket threading graph "../utils.rkt")

(match-define (list grid-width grid-height num-corruptions) (list 70 70 1024))

(define byte-positions
  (~>> (get-aoc 2024 18) (string-split _ "\n") (map (λ~>> (string-split _ ",") (map string->number)))))

(define (get-nearby/2D x y #:dirs [dirs directions/nesw] #:bound? [bound? #t])
  (define (safe? nx ny) (if bound? (and (<= 0 nx grid-width) (<= 0 ny grid-height)) #t))
  (filter (curry apply safe?) (map (λ~> (map + _ (list x y))) dirs)))

(define RAM-graph
  (let ([graph-base (for*/list ([x (in-inclusive-range 0 grid-width)] [y (in-inclusive-range 0 grid-height)]) (list x y))])
    (unweighted-graph/adj (for/list ([node graph-base]) (cons node (apply get-nearby/2D node))))))

;; pt 1
(for ([corruption (in-list (take byte-positions 1024))])
  (remove-vertex! RAM-graph corruption))

(~> RAM-graph (fewest-vertices-path (list 0 0) (list grid-width grid-height)) length sub1)

;; pt 2
(for/first ([corruption (in-list (drop byte-positions 1024))]
            #:do [(remove-vertex! RAM-graph corruption)]
            #:unless (fewest-vertices-path RAM-graph (list 0 0) (list grid-width grid-height)))
  corruption)
