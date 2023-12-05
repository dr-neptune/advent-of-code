#lang racket
(require racket advent-of-code)

(define bag-draws
  (map (λ (s) (map string-trim (string-split s ";")))
       (string-split (fetch-aoc-input (find-session) 2023 2) "\n")))

(define (flatten-depth lst [depth 1])
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(define (val-filter ht predicate)
  (for/hash ([(k v) (in-hash ht)] #:when (predicate v)) (values k v)))

;; part 1
(define (parse-draws draw-string)
  #| "2 blue, 1 red, 2 green" -> '((2 . "blue") (1 . "red") (2 . "green")) |#
  (map
   (λ (s) (let ([vals (string-split s)])
            (cons (string->number (first vals))
                  (last vals))))
   (map string-trim (string-split draw-string ","))))

(define game-hash
  (let ([games bag-draws])
    (for*/hash ([game games]
                [draw game])
      (let ([game-match (regexp-match* #px"Game \\d+" draw)])
        (if (not (empty? game-match))
            (let ([first-draw (regexp-replace* #px"Game \\d+: " draw "")])
              (values game-match (cons (parse-draws first-draw)
                                       (map parse-draws (rest game)))))
            (values game-match (parse-draws draw)))))))

(define draw-max-counts
  (for/hash ([game (filter (compose not empty?) (hash-keys game-hash))])
    (values game
            (map (λ (ls) (apply max (map car ls)))
                 (group-by cdr (sort (flatten-depth (hash-ref game-hash game))
                                     #:key cdr string<?))))))
(apply + (map
          (compose string->number cadr string-split)
          (flatten (hash-keys (val-filter draw-max-counts (λ (val) (andmap <= val '(14 13 12))))))))

;; part 2
(apply + (hash-map draw-max-counts (λ (k v) (apply * v))))
