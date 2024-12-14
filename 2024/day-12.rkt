#lang racket
(require racket threading "../utils.rkt")

(define garden (~> (get-aoc 2024 12) string->grid/2D))
(define get-nearby-pts (curry get-nearby/2D garden))
(define (get-plant pt) (get-cell/2D garden (first pt) (second pt)))

(define (dfs pt)
  (let ((plant-type (get-plant pt)))
    (define (dfs-aux pt visited)
      (cond
        [(not (equal? (get-plant pt) plant-type)) visited]
        [(set-member? visited pt) visited]
        [else
         (let ((new-visited (set-add visited pt)))
           (foldl dfs-aux new-visited (apply get-nearby-pts pt)))]))
    (dfs-aux pt (set))))

(define (sub-spaces garden)
  (let ([master-accumulator '()] [visited (set)])
    (for* ([x (in-range (vector-length (vector-ref garden 0)))]
           [y (in-range (vector-length garden))])
      (when (not (set-member? (list x y) visited))
        (let ([res (dfs (list x y))])
          (when (false? (member res master-accumulator))
            (begin
              (set! visited (set-union res visited))
              (set! master-accumulator (cons res master-accumulator)))))))
    master-accumulator))

(define (get-perimeter shape-set)
  (for*/sum ([pt (in-set shape-set)]
             [nearby (get-nearby-pts (first pt) (second pt) #:bound? #f)]
             #:when (false? (set-member? shape-set nearby)))
    1))

(define (get-vertices surroundings)
  (match-let ([(list n ne e se s sw w nw) surroundings])
    (list (and (not n) (not w)) (and (not n) (not e))
          (and (not s) (not w)) (and (not s) (not e))
          (and n w (not nw)) (and n e (not ne))
          (and s w (not sw)) (and s e (not se)))))

(define (get-corners shape-set)
  (for/sum ([pt (in-set shape-set)])
    (~>> (apply (curry get-nearby-pts #:dirs directions+diag #:bound? #f) pt)
                         (map (curry set-member? shape-set))
                         get-vertices
                         (count identity))))

(define fences (sub-spaces garden))
(dot-product (map set-count fences) (map get-perimeter fences))  ;; pt 1 => 1461806
(dot-product (map set-count fences) (map get-corners fences))    ;; pt 2 => 887932
