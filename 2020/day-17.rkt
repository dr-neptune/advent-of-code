#lang racket
(require racket threading advent-of-code)

(pretty-print-columns 25)

(define cubes
  (~>>
;; ".#.
;; ..#
;; ###"
 (fetch-aoc-input (find-session) 2020 17)
   (string-split _ "\n")
   (map string->list)))

(define neighbor-offsets
  (for*/list ([dx (in-list '(-1 0 1))]
              [dy (in-list '(-1 0 1))]
              [dz (in-list '(-1 0 1))]
              #:when (not (and (= dx 0) (= dy 0) (= dz 0))))
    (list dx dy dz)))

(define points (make-hash))

(for* ([y (length cubes)]
       [x (in-range (length (first cubes)))]
       #:do [(define val (list-ref (list-ref cubes y) x))]
       #:when (char=? val #\#))
  (hash-set! points (list x y 0) val))

(define (count-active-neighbors active-cubes)
  (let ([neighbor-counts (make-hash)])
    (for* ([cube (in-hash-keys active-cubes)]
           [offset neighbor-offsets])
      (let ([neighbor (map + cube offset)])
        ;; possible bug here. Should we init with 0 or 1?
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
    (set! current-active (next-cycle current-active)))
  current-active)

;; part 1
(hash-count (simulate-cycles points 6))

;; part 2
