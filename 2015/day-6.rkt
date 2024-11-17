#lang racket
(require racket threading advent-of-code)

(define (parse-line str)
  (let* ([regex #px"^(toggle|turn off|turn on) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$"]
         [match (regexp-match regex str)])
    (if match
        (list
         (list-ref match 1) ; Action (preserved as in input)
         (list (string->number (list-ref match 2)) ; First X
               (string->number (list-ref match 3))) ; First Y
         (list (string->number (list-ref match 4)) ; Second X
               (string->number (list-ref match 5)))) ; Second Y
        #f)))

(define light-instr (~> (fetch-aoc-input (find-session) 2015 6)
                        (string-split "\n")
                        (map parse-line _)))


#|

idea

make a 1000x1000 vector grid

parse each instruction
dispatch a function for turn on / turn off / toggle
make a function that converts number pairs to indices

idea

could we do this with just a 1d vector? would that be easier?
we could say off is 0, on is 1

|#
(define lights (make-vector 1000000 0))

(define (coord-to-index-row-major x y)
  (+ (* y 1000) x))

(define (perform-op op start-coord end-coord)
  (let ([start-coord (apply coord-to-index-row-major start-coord)]
        [end-coord (apply coord-to-index-row-major end-coord)])
    (match op
      ["turn on" (for ([i (in-inclusive-range start-coord end-coord)])
                   (vector-set! lights i 1))]
      ["turn off" (for ([i (in-inclusive-range start-coord end-coord)])
                    (vector-set! lights i 0))]
      ["toggle" (for ([i (in-inclusive-range start-coord end-coord)])
                  (if (equal? (vector-ref lights i) 1)
                      (vector-set! lights i 0)
                      (vector-set! lights i 1)))])))

(for ([instr light-instr])
  (match-let ([(list op start end) instr])
    (perform-op op start end)))

(sequence-fold + 0 lights)
