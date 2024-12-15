#lang racket
(require racket threading math/statistics "../utils.rkt")

(define robots
  (~> (get-aoc 2024 14) (string-split "\n")
      (map (λ~>> (regexp-match* #px"[+-]?\\d+") (map string->number)) _)))

;; (match-define (list grid-width grid-height) (list 11 7))
(match-define (list grid-width grid-height) (list 101 103))

(pretty-print-columns (* 10 grid-width))

(define (calculate-loc s x y vx vy)
  (let rec ([s s] [x x] [y y])
    (if (zero? s) (list x y)
        (let ([new-x (+ x vx)] [new-y (+ y vy)]
              [circle (λ (v dist) (if (negative? v) (+ dist v) (remainder v dist)))])
          (rec (sub1 s) (circle new-x grid-width) (circle new-y grid-height))))))

(define (quadrant ele)
  (match-let* ([(list ele-x ele-y) ele]
               [v-/ (curryr quotient 2)]
               [x-/ (v-/ grid-width)]
               [y-/ (v-/ grid-height)])
    (cond [(and (< ele-x x-/) (< ele-y y-/)) 1]
          [(and (< x-/ ele-x) (< ele-y y-/)) 2]
          [(and (< ele-x x-/) (< y-/ ele-y)) 3]
          [(and (< x-/ ele-x) (< y-/ ele-y)) 4]
          [else 0])))

;; part 1
(~>> robots
     (map (λ~>> (apply (curry calculate-loc 100)) quadrant))
     (group-by identity)
     (filter (λ~> car zero? not))
     (map length)
     (apply *))

;; part 2
(define (make-grid-display coords s [max-x grid-width] [max-y grid-height])
  (match-define (list green red reset) (list "\x1b[32m" "\x1b[31m" "\x1b[0m"))

  (define grid
    (for/vector ([row (in-range (add1 max-y))])
      (make-vector (add1 max-x) " ")))

  (for ([pt coords])
    (define x (first pt))
    (define y (second pt))
    (vector-set! (vector-ref grid y) x (string-append green "^" reset)))

  (define divider (make-vector (+ 4 grid-width) (string-append red "-" reset)))

  (displayln divider)
  (displayln (string-append green "\t\t\t\tIteration: " (number->string s) reset))
  (displayln "")
  (pretty-display grid)
  (displayln "")
  (displayln divider))

(define (median-of-pairwise-distances points #:sample-num [sample-num #f])
  (define (pairwise-distances points)
    (for*/list ([i (in-range (length points))]
                [j (in-range (add1 i) (length points))])
      (let* ([p1 (list-ref points i)] [x1 (first p1)] [y1 (second p1)]
             [p2 (list-ref points j)] [x2 (first p2)] [y2 (second p2)]
             [dx (- x1 x2)] [dy (- y1 y2)])
        (sqrt (+ (* dx dx) (* dy dy))))))
  (median < (pairwise-distances (if (number? sample-num) (take (shuffle points) sample-num) points))))

(for/fold ([robots robots] [min-dist-median +Inf.0])
          ([s (in-range 10000)])
  (let* ([new-robos (map (λ (robot) (append (apply calculate-loc (cons 1 robot)) (take-right robot 2))) robots)]
         [robo-points (map (curryr take 2) new-robos)]
         [pairwise-dist-median (median-of-pairwise-distances (take (shuffle robo-points) 100))])
    (when (< pairwise-dist-median min-dist-median)
      (make-grid-display robo-points s))

    (values new-robos (min pairwise-dist-median min-dist-median))))
