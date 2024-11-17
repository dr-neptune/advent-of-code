#lang racket
(require racket threading advent-of-code)

(define (parse-line str)
  (let* ([regex #rx"^(toggle|turn off|turn on) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$"]
         [match (regexp-match regex str)])
    (if match
        (list
         (list-ref match 1) ; Action (case preserved)
         (list (string->number (list-ref match 2)) ; First X
               (string->number (list-ref match 3))) ; First Y
         (list (string->number (list-ref match 4)) ; Second X
               (string->number (list-ref match 5)))) ; Second Y
        #f)))

(define instr
  (~> (fetch-aoc-input (find-session) 2015 6)
      (string-split "\n")
      (map parse-line _)))

(define grid (for/vector ([_ 1000])
               (make-vector 1000 0)))

(define (get-coords start end)
  (match-let ([(list x1 y1) start]
              [(list x2 y2) end])
    (for*/list ([i (in-inclusive-range x1 x2)]
                [j (in-inclusive-range y1 y2)])
      (list i j))))

(define (set-grid! x y val)
  (vector-set! (vector-ref grid y) x val))

(define (grid-ref x y)
  (vector-ref (vector-ref grid y) x))

;; part 1
(define (perform-op op start-coord end-coord)
  (let ([coords (get-coords start-coord end-coord)])
    (for ([coord coords]
          #:do [(match-define (list x y) coord)])
      (match op
        ["turn on" (set-grid! x y 1)]
        ["turn off" (set-grid! x y 0)]
        ["toggle" (if (zero? (grid-ref x y))
                      (set-grid! x y 1)
                      (set-grid! x y 0))]))))

(for ([instr instr])
  (apply perform-op instr))

(sequence-fold + 0 (vector-map (λ (vec) (sequence-fold + 0 vec)) grid))


;; part 2
(define grid (for/vector ([_ 1000]) (make-vector 1000 0)))

(define (perform-op op start-coord end-coord)
  (let ([coords (get-coords start-coord end-coord)])
    (for ([coord coords]
          #:do [(match-define (list x y) coord)])
      (match op
        ["turn on" (set-grid! x y (+ (grid-ref x y) 1))]
        ["turn off" (set-grid! x y (max 0 (- (grid-ref x y) 1)))]
        ["toggle" (set-grid! x y (+ (grid-ref x y) 2))]))))

(for ([instr instr])
  (apply perform-op instr))

(sequence-fold + 0 (vector-map (λ (vec) (sequence-fold + 0 vec)) grid))
