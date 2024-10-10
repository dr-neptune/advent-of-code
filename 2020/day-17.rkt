#lang racket
(require racket threading advent-of-code)

(define cubes
  (~>>
   (fetch-aoc-input (find-session) 2020 17)
   (string-split _ "\n")
   (map string->list)))

(define DIMENSION 5)

(define (make-neighbor-offsets dim)
  (~>> (make-list dim '(-1 0 1))
       (apply cartesian-product)
       (filter (λ~> (equal? (make-list dim 0)) not))))

(define neighbor-offsets (make-neighbor-offsets DIMENSION))

(define points
  (for*/hash ([y (length cubes)]
              [x (in-range (length (first cubes)))]
              #:do [(define val (list-ref (list-ref cubes y) x))]
              #:when (char=? val #\#))
    (values (append (list x y) (make-list (- DIMENSION 2) 0)) val)))

(define (count-active-neighbors active-cubes)
  (let ([neighbor-counts (make-hash)])
    (for* ([cube (in-hash-keys active-cubes)]
           [offset neighbor-offsets])
      (let ([neighbor (map + cube offset)])
        (hash-update! neighbor-counts neighbor add1 0)))
    neighbor-counts))

(define (change-state pt-state actives-len)
  (match pt-state
    [#\# (match actives-len [(or 2 3) #\#] [_ #\.])]
    [#\. (match actives-len [3 #\#] [_ #\.])]))

(define (next-step active-cubes)
  (let ([neighbors (count-active-neighbors active-cubes)])
    (for/hash ([(pt n-count) (in-hash neighbors)]
               #:do [(define curr-state (if (hash-has-key? active-cubes pt) #\# #\.))
                     (define next-state (change-state curr-state n-count))]
               #:when (char=? next-state #\#))
      (values pt #\#))))

(define (simulate-cycles active-cubes cycles)
  (define current-active active-cubes)
  (for ([i (in-range cycles)])
    (set! current-active (next-step current-active)))
  current-active)

;; part 1
(hash-count (simulate-cycles points 6))

#|

part 2 is the same as above, except that we extend the dimensions to
include the 4th dimension, w. All you need to do is set DIMENSION to 4
and rerun the above.

The dim=5 case takes about an order of magnitude longer

|#
