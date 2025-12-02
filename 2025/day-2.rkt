#lang racket
(require racket advent-of-code threading)

;; fetch puzzle ranges and expand them into inclusive streams.
(define ranges (fetch-aoc-input (find-session) 2025 2))

(define i-ranges
  (~> ranges
      (regexp-match* #px"(\\d+)-(\\d+)" _ #:match-select cdr)
      (map (λ~>> (map string->number)
                 (apply in-inclusive-range)) _)))

;; part 1
;; Check if a number’s digits split evenly into k equal chunks.
(define (tandem? str [k 2])
  (let* ([len (string-length str)]
         [chunk (quotient len k)])
    (and (= len (* chunk k))
         (for/and ([i (in-range 1 k)])
           (string=? (substring str 0 chunk)
                     (substring str (* i chunk) (* (+ i 1) chunk)))))))

(for*/sum ([rng (in-list i-ranges)]
           [val (in-stream rng)]
           #:when (tandem? (number->string val)))
  val)

;; part 2
;; Return #t if the string is tiled by a shorter period (exact repetition).
(define (periodic? str)
  (let* ([n (string-length str)])
    (for/or ([k (in-inclusive-range 2 n)]
             #:when (zero? (remainder n k))
             #:when (tandem? str k))
      #t)))

(for*/sum ([rng (in-list i-ranges)]
           [val (in-stream rng)]
           #:when (periodic? (number->string val)))
  val)
