#lang racket
(require racket threading memo "../utils.rkt")

(match-define (list city city-width city-height antennae)
  (let* ([inp (get-aoc 2024 8)]
         [city (string->grid/2D inp)]
         [width (vector-length (vector-ref city 0))] [height (vector-length city)])
    (list city width height (~> inp string->list remove-duplicates (remq* (list #\newline #\.) _)))))

(define (within-bounds x y) (and (< -1 x city-width) (< -1 y city-height)))

(define (reflect coord origin)
  (match-let* ([(list coord-x coord-y origin-x origin-y) (append coord origin)]
               [reflect-coord (λ (coord-val origin-val) (- (* 2 origin-val) coord-val))]
               [refl (list (reflect-coord coord-x origin-x) (reflect-coord coord-y origin-y))])
    (if (apply within-bounds refl) refl '())))

(define (get-antipodes antenna-char [reflect-fn reflect])
  (let* ([satellite-locations (get-locations/2D city antenna-char)])
    (flatten/1
     (for/list ([satellite satellite-locations])
       (let* ([other-satellites (remove satellite satellite-locations)])
         (for/list ([other-satellite other-satellites])
           (reflect-fn other-satellite satellite)))))))

;; part 1
(~> (map get-antipodes antennae) flatten/1 (filter (compose not empty?) _) remove-duplicates length)

;; part 2
(define (reflect* coord origin)
  (let ([refl (reflect coord origin)])
    (if (empty? refl) '() (cons refl (reflect* origin refl)))))

(~> (append (~> (map (curryr get-antipodes reflect*) antennae) flatten/2)
            (~> (map (curry get-locations/2D city) antennae) flatten/1))
    remove-duplicates
    length)
