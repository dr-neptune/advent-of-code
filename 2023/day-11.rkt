#lang racket
(require racket advent-of-code)

(define image (string-split (fetch-aoc-input (find-session) 2023 11) "\n"))


(define image (string-split "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....." "\n"))

#|

idea
1. double the rows that contain no galaxies
2. get the coordinates of every #
3. get all combinations (i.e. for pairs a through z, get ab, ac, ..., az, bc, bd, ..., bz, cd, ..., cz, ...)
4. get manhattan distance between each pair

|#

(define (transpose lst)
  (apply map list lst))

(define (expand-space image)
  ;; add horizontally
  (define (h-expand image)
    (for/fold ([acc '()]
               #:result (reverse acc))
              ([segment image])
      (if (not (string-contains? segment "#"))
          (values (cons segment (cons segment acc)))
          (values (cons segment acc)))))
  ;; add vertically
  (define (v-expand image)
    (let ([image (map string->list image)])
      (for/fold ([acc '()]
                 #:result (map list->string (transpose (reverse acc))))
                ([col (in-range (length (first image)))])
        ;; get all items from col
        (let ([column (map (curryr list-ref col) image)])
          (if (false? (member #\# column))
              (cons column (cons column acc))
              (cons column acc))))))
  ((compose v-expand h-expand) image))



(define expanded-image (expand-space image))

; get all the coordinates of each #

(define (matrix-indexes-of matrix pred)
  (define (row-indexes-of row pred y x res)
    (cond
      [(null? row) res]
      [(pred (car row)) (row-indexes-of (cdr row) pred y (add1 x) (cons (list x y) res))]
      [else (row-indexes-of (cdr row) pred y (add1 x) res)]))

  (let loop ([mtrx matrix] [y 0] [res '()])
    (cond
      [(null? mtrx) (reverse res)]
      [else (append (row-indexes-of (car mtrx) pred y 0 '())
                    (loop (cdr mtrx) (add1 y) res))])))

(define star-coordinates (matrix-indexes-of (map string->list expanded-image) (curry equal? #\#)))

(define (manhattan-distance a b)
  (match-let ([(list (list x1 y1) (list x2 y2)) (list a b)])
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(for/sum ([combo (in-combinations star-coordinates 2)])
  (match-let ([(list a b) combo])
    ;; (displayln (format "~a ~a ~a" a b (manhattan-distance a b)))
    (manhattan-distance a b)))


;; part 2
(define (expand-space image)
  ;; add horizontally
  (define (h-expand image)
    (for/fold ([acc '()]
               #:result (reverse acc))
              ([segment image])
      (if (not (string-contains? segment "#"))
          (values (append (make-list 1000000 segment) acc))
          (values (cons segment acc)))))
  ;; add vertically
  (define (v-expand image)
    (let ([image (map string->list image)])
      (for/fold ([acc '()]
                 #:result (map list->string (transpose (reverse acc))))
                ([col (in-range (length (first image)))])
        ;; get all items from col
        (let ([column (map (curryr list-ref col) image)])
          (if (false? (member #\# column))
              (append (make-list 1000000 column) acc)
              ;; (cons column (cons column acc))
              (cons column acc))))))
  ((compose v-expand h-expand) image))

(define expanded-image (expand-space image))
(define star-coordinates (matrix-indexes-of (map string->list expanded-image) (curry equal? #\#)))

(for/sum ([combo (in-combinations star-coordinates 2)])
  (match-let ([(list a b) combo])
    ;; (displayln (format "~a ~a ~a" a b (manhattan-distance a b)))
    (manhattan-distance a b)))
