#lang racket
(require racket threading advent-of-code)

(define opts '(20 15 10 5 5))
(define target 25)

;; (define opts '(11 30 47 31 32 36 3 1 5 3 32 36 15 11 46 26 28 1 19 3))
;; (define target 150)

(define (combos containers target)
  (cond [(zero? target) (list '())]
        [(or (negative? target) (empty? containers)) '()]
        [else
         (let* ([fopt (first containers)]
                [ropt (rest containers)]
                [with-fopt (combos ropt (- target fopt))]
                [without-fopt (combos ropt target)]
                [with-fopt-combos
                  (map (λ (combo) (cons fopt combo)) with-fopt)])
           (append with-fopt-combos without-fopt))]))

;; part 1
(define possible-combos (combos opts target))
(length possible-combos)

;; part 2
(let ([min-size (apply min (map length possible-combos))])
  (~>> possible-combos
       (filter (λ (combo) (= (length combo) min-size)))
       length))
