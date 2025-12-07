#lang racket
(require racket advent-of-code)

;; Puzzle input for Day 4.
(define paper (fetch-aoc-input (find-session) 2025 4))

;; Convert a newline-delimited string into a vector-of-vector grid of characters.
(define (string->grid s)
  (list->vector
   (for/list ([line (in-list (string-split s "\n" #:trim? #t))])
     (list->vector (string->list line)))))

(define (grid-height grid) (vector-length grid))
(define (grid-width grid) (vector-length (vector-ref grid 0)))
(define (grid-cell grid x y) (vector-ref (vector-ref grid y) x))

;; Return the values of the 8 surrounding cells around (x, y) that stay in bounds.
(define (adjacent-8 grid x y)
  (let* ([height (grid-height grid)] [width (grid-width grid)]
         [directions '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1))])
    (for*/list ([dir (in-list directions)]
                #:do [(match-define (list dx dy) dir)
                      (define nx (+ x dx))
                      (define ny (+ y dy))]
                #:when (and (<= 0 nx) (< nx width) (<= 0 ny) (< ny height)))
      (grid-cell grid nx ny))))

(define grid (string->grid paper))

(define (accessible? grid x y)
  (and (eq? (grid-cell grid x y) #\@)
       (< (count (curry eq? #\@) (adjacent-8 grid x y)) 4)))

(define (get-accessible-rolls grid)
  (let ([height (grid-height grid)] [width (grid-width grid)])
    (for*/list ([y (in-range height)] [x (in-range width)]
                #:when (accessible? grid x y))
      (list x y))))

;; part 1
(for*/sum ([y (in-range (grid-height grid))]
           [x (in-range (grid-width grid))]
           #:when (accessible? grid x y))
  1)

;; part 2
(define (update-grid coord grid)
  (match-let ([(list x y) coord])
    (let* ([row (vector-ref grid y)]
           [row* (vector-copy row)]
           [_ (vector-set! row* x #\.)]
           [grid* (vector-copy grid)])
      (vector-set! grid* y row*)
      grid*)))

(let loop ([total-rolls 0] [g grid])
  (let* ([accessible-rolls (get-accessible-rolls g)]
         [num-acc (length accessible-rolls)])
    (if (zero? num-acc)
        total-rolls
        (loop (+ total-rolls num-acc)
              (foldl update-grid g accessible-rolls)))))
