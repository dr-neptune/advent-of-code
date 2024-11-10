#lang racket
(require racket racket/hash threading advent-of-code)

(define tile-map
  (~> (fetch-aoc-input (find-session) 2020 24)
   (string-split "\n")))

;; parse string into individual directoins
(define (parse-directions input)
  (regexp-match* #rx"(se|sw|ne|nw|e|w)" input))


;; part 1
(define axial-coordinate-diff-hash
  ;; (dq dr) where q is northwest and r is east
  ;; see https://www.redblobgames.com/grids/hexagons/
  (make-hash '(("ne" (1 -1))
               ("se" (0 1))
               ("sw" (-1 1))
               ("nw" (0 -1))
               ("w" (-1 0))
               ("e" (1 0)))))

(define (get-final-location str)
  (let ([parsed-string (parse-directions str)])
    (foldl (λ (pair orig) (list (+ (first pair) (first orig))
                                (+ (second pair) (second orig))))
           (list 0 0)
           (map (compose first (curry hash-ref axial-coordinate-diff-hash)) parsed-string))))

(define (val->color val)
  (if (odd? val) 'black 'white))

(define location-visits
  (let ([hsh (make-hash)])
    (for ([p (map get-final-location tile-map)])
      (hash-update! hsh p add1 0))
    ;; set odds to black, evens to white
    (for ([(loc val) (in-hash hsh)])
      (hash-set! hsh loc (val->color val)))
    hsh))

(hash-count (hash-filter-values location-visits (curry equal? 'black)))

;; part 2
(define neighbor-offsets '((1 -1) (1 0) (0 1) (-1 1) (-1 0) (0 -1)))

(define (get-neighbors loc)
  (map (λ (offset)
         (map + loc offset))
       neighbor-offsets))

(define (rule-check curr-color black-count)
  (match curr-color
    ['black (if (or (equal? black-count 0) (> black-count 2))
                'white
                'black)]
    ['white (if (equal? black-count 2)
                'black
                'white)]))

(define (add-peripherals black-tiles)
  (let ([tiles-set (make-hash)])
    (for ([tile (in-hash-keys black-tiles)])
      (begin
        (hash-set! tiles-set tile 'black)
        (for ([neighbor (get-neighbors tile)])
          (unless (hash-has-key? tiles-set neighbor)
            (hash-set! tiles-set neighbor 'white)))))
    tiles-set))

(define is-black? (curry equal? 'black))

(define (next-step black-tiles)
  (let ([loc-with-periph (add-peripherals black-tiles)]
        [new-hsh (make-hash)])
    (for/list ([(loc color) (in-hash loc-with-periph)])
      (let* ([adjacents (get-neighbors loc)]
             [black-adj (hash-filter-values
                         (hash-filter-keys loc-with-periph (λ (k) (member k adjacents)))
                         (curry equal? 'black))]
             [black-count (hash-count black-adj)])
        (let ([new-color (rule-check color black-count)])
          (hash-set! new-hsh loc new-color))))
    new-hsh))

(let ([lobby-layout (hash-filter-values location-visits is-black?)])
  (for ([idx (in-inclusive-range 0 100)])
    (when (equal? (remainder idx 10) 0)
      (displayln (format "idx: ~a num-blacks: ~a" idx (hash-count (hash-filter-values lobby-layout is-black?)))))
    (set! lobby-layout
          (hash-filter-values (next-step lobby-layout) is-black?)))
  (hash-count (hash-filter-values lobby-layout is-black?)))
