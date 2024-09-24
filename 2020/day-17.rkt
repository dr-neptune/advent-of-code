#lang racket
(require racket threading)

(pretty-print-columns 25)

(define cubes
  (~>>
".#.
..#
###"
   (string-split _ "\n")
   (map string->list)))


#|

given x = 1
we can take x + 1 and x - 1 if x - 1 is not < 0
similarly for y, z
|#

;; (define (get-neighbors x y z)
;;   (define-flow neighbor-vals
;;     (~> (-< (~> add1 (max 0))
;;             (~> sub1 (max 0))) ▽))
;;   (match-let ([(list xs ys zs) (~> ((list x y z)) (△ neighbor-vals) ▽)])
;;     (for*/list ([x xs] [y ys] [z zs])
;;       (list x y z))))

;; (get-neighbors 1 2 3)

#|

oh my, this is infinite in all 3 directions. I guess we don't care about +- x, y, z
we just want to build out a dynamic universe of objects and for each iteration generate
the next state
|#


#|

idea

instead of simulating in place, what if we go only off of generating indices nearby?

start with z=0
then we have (x, y, 0, symbol) for the initial grid
add them to a hashmap, maybe {(x, y, z): sym}
we can find each active cell by filtering to symbol = #\#

then for each element that is active, we can get all of its neighbors
if the neighbor doesn't exist, add it to a dict of points

|#


(define neighbor-offsets
  (for*/list ([dx (in-list '(-1 0 1))]
              [dy (in-list '(-1 0 1))]
              [dz (in-list '(-1 0 1))]
              #:when (not (and (= dx 0) (= dy 0) (= dz 0))))
    (list dx dy dz)))

(pretty-print neighbor-offsets)

(define points (make-hash))

(for* ([y (length cubes)]
       [x (in-range (length (first cubes)))]
       #:do [(define val (list-ref (list-ref cubes y) x))])
  (hash-set! points (list x y 0) val))

(pretty-print points)

#|

now we have the initial active state cubes in the hash
and we have a list of neighbor offsets.

next, we should get all the neighbor points for our initial state
then see which rules apply

need to add inactive keys as well!

|#

;; get inactive and active neighboring points

;; note: might have to do memoization later
;;       for the generation of actives list

(define (change-state pt-state actives-len)
  (match pt-state
    [#\# (match actives-len [(or 2 3) #\#] [_ #\.])]
    [#\. (match actives-len [3 #\#] [_ #\.])]))


;; make intermediate change map
(define change-map
  (for/hash ([(pt state) (in-hash points)])
    (let* ([neighbors (for/list ([offset neighbor-offsets]) (map + pt offset))]
           [actives (filter
                     (λ (val)
                       (let ([hr (hash-ref points val #f)])
                         (if hr (equal? hr #\#) #f)))
                     neighbor-offsets)])
      (displayln (format "pt: ~a \t state: ~a \t nearby: ~a \t neighbors: ~a" pt state actives neighbors))
      (values pt (change-state state (length actives)))
      )))

;; we were applying an offset
;; maybe we should assume each point is 0 0 0 and just iterate over core offsets

;; apply all at once
(for ([(pt new-val) (in-hash change-map)])
  (hash-set! points pt new-val))

(pretty-print points)

(pretty-print change-map)

;; we need to grow the grid!

;; example of error
;; for 1 1 0 we start with .
;; it seems to have 5 active cells around it
;; but in cycle 1, it has flipped, indicating only 3 count
;; ensure that we aren't hitting diagonals?

(pretty-print (first inter))
(pretty-print (second inter))

(pretty-print neighbor-offsets)

(let ([pt-k '(2 1 0)]
      [pt-v #\#]
      [actives (first inter)]
      [inactives (second inter)])
  (match pt-v
    [#\# (if (or (equal? 2 (length actives))
                 (equal? 3 (length actives)))
             #\#
             #\.)]
    [#\. (if (equal? 3 (length actives))
             #\#
             #\.)]))
