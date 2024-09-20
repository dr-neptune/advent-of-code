#lang racket
(require racket qi)

(pretty-print-columns 1)

(define cubes
  (~>
   (".#.
..#
###")
   (string-split "\n")
   (△ string->list)
   ▽))

(define-flow make-blank-index
  (~>
    (-< length
        (~> first length (make-list #\.)))
    make-list))

(let* ([z0 cubes]
       [z1 (make-blank-index cubes)]
       [z2 (make-blank-index cubes)])
  (list z0 z1 z2))


#|

given x = 1
we can take x + 1 and x - 1 if x - 1 is not < 0
similarly for y, z
|#

(define (get-neighbors x y z)
  (define-flow neighbor-vals
    (~> (-< (~> add1 (max 0))
            (~> sub1 (max 0))) ▽))
  (match-let ([(list xs ys zs) (~> ((list x y z)) (△ neighbor-vals) ▽)])
    (for*/list ([x xs] [y ys] [z zs])
      (list x y z))))

(get-neighbors 1 2 3)
