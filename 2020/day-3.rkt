#lang racket
(require racket threading advent-of-code)

(define tree-grid (~> (fetch-aoc-input (find-session) 2020 3) (string-split "\n")))
(define tree-grid (string-split tree-grid "\n"))


;; (define exstr
;;   "..##.........##.........##.........##.........##.........##.......
;; #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
;; .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
;; ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
;; .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
;; ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
;; .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
;; .#........#.#........#.#........#.#........#.#........#.#........#
;; #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
;; #...##....##...##....##...##....##...##....##...##....##...##....#
;; .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#")

;; (define tree-grid (string-split exstr "\n"))

;;  The tree pattern repeats itself infinitely to the right!
(for/fold ([points '()]
           [curr-point 3]
           #:result (count (curry equal? #\#) points))
          ([tree-line (rest tree-grid)]
           #:do [(define trees (string->list tree-line))])
  (values (cons (list-ref trees curr-point) points)
          (remainder (+ curr-point 3) (length trees))))

;; part 2

(define (get-slope-count right down)
  (for/fold ([points '()]
             [curr-point right]
             [idx 0]
             #:result (count (curry equal? #\#) points))
            ([tree-line (rest tree-grid)]
             #:do [(define trees (string->list tree-line))])
    (if (zero? (remainder idx down))
        (values (cons (list-ref trees curr-point) points)
                (remainder (+ curr-point right) (length trees))
                (add1 idx))
        (values points curr-point (add1 idx)))))


(* (get-slope-count 1 1)
   (get-slope-count 3 1)
   (get-slope-count 5 1)
   (get-slope-count 7 1)
   (get-slope-count 1 2))
