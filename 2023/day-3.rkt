#lang racket
(require racket racket/hash advent-of-code)

(define schematic (string-split (fetch-aoc-input (find-session) 2023 3) "\n"))

(define schematic
  (string-split "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.." "\n"))


#|

idea

find symbols coordsd
check adjacent cell coords to see if a number exists
if so, grab it

|#

(define (row-indexes-of row pred y x res)
  (cond
   [(null? row) res]
   [(pred (car row)) (row-indexes-of (cdr row) pred y (add1 x) (cons (list x y) res))]
   [else (row-indexes-of (cdr row) pred y (add1 x) res)]))

(define (matrix-indexes-of matrix pred)
  (let loop ([mtrx matrix] [y 0] [res '()])
    (cond
      [(null? mtrx) (reverse res)]
      [else (append (row-indexes-of (car mtrx) pred y 0 '())
                    (loop (cdr mtrx) (add1 y) res))])))

(define (not-alpha-numeric-or-dot? char)
  (not (char-alphanumeric? char)))

(define (char-alphanumeric? char)
  (or (char-numeric? char) (char-alphabetic? char) (char=? char #\.)))

(define (are-coordinates-adjacent? coord1 coord2)
  (let* ([x1 (car coord1)]
         [y1 (cadr coord1)]
         [x2 (car coord2)]
         [y2 (cadr coord2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))])
    (and (<= dx 1) (<= dy 1))))

(define (matrix-ref lst x y)
  (string-ref (list-ref lst y) x))

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (list->number lst)
  (let loop ([lst lst] [result 0])
    (if (null? lst)
        result
        (loop (cdr lst) (+ (* result 10) (car lst))))))

(define (find-numbers-and-coordinates str [y-val 0])
  (let loop ([chars (string->list str)]
             [index 0]
             [current-number '()]
             [numbers '()]
             [coordinates '()])
    (cond
      [(null? chars)
       (if (null? current-number)
           numbers
           (reverse (cons (list (list->number (reverse current-number)) (reverse coordinates)) numbers)))]
      [(char-numeric? (car chars))
       (loop (cdr chars)
             (add1 index)
             (cons (char->digit (car chars)) current-number)
             numbers
             (cons (list index y-val) coordinates))]
      [else
       (loop (cdr chars)
             (add1 index)
             '()
             (if (null? current-number)
                 numbers
                 (reverse (cons (list (list->number (reverse current-number)) (reverse coordinates)) numbers)))
             '())])))

(define (convert-to-hashmap num-coord-list)
  (let ([hashmap (make-hash)])
    (for-each (lambda (num-coord-pair)
                (when (not (null? num-coord-pair)) ; Skip empty lists
                  (let* ([number (car num-coord-pair)]
                         [coords (cadr num-coord-pair)])
                    (for-each (lambda (coord)
                                (hash-set! hashmap coord number))
                              coords))))
              num-coord-list)
    hashmap))

(define symbol-indices
  (matrix-indexes-of
    (map string->list schematic)
    not-alpha-numeric-or-dot?))

(define num-indices
  (matrix-indexes-of
    (map string->list schematic)
    char-numeric?))

(define num-coords
  (map find-numbers-and-coordinates schematic (stream->list (in-range (length schematic)))))

(((467 ((0 0) (1 0) (2 0))) (114 ((5 0) (6 0) (7 0))))
 ()
 ((35 ((2 2) (3 2))) (633 ((6 2) (7 2) (8 2))))
 ()
 ((617 ((0 4) (1 4) (2 4))))
 ((58 ((7 5) (8 5))))
 ((592 ((2 6) (3 6) (4 6))))
 ((755 ((6 7) (7 7) (8 7))))
 ()
 ((664 ((1 9) (2 9) (3 9))) (598 ((5 9) (6 9) (7 9)))))

(define (num-coord-hash schematic)
  (let ([schematics-coords
         (map find-numbers-and-coordinates schematic (stream->list (in-range (length schematic))))])
    (let ([hsh (make-hash)])
      (begin
        (for ([h (map convert-to-hashmap schematics-coords)])
          (hash-union! hsh h))
        hsh))))

(define coord->number (num-coord-hash schematic))


'(((1 9) . 664)
  ((8 5) . 58)
  ((3 2) . 35)
  ((6 7) . 755)
  ((5 0) . 114)
  ((2 4) . 617)
  ((3 9) . 664)
  ((6 9) . 598)
  ((4 6) . 592)
  ((7 7) . 755)
  ((8 2) . 633)
  ((1 4) . 617)
  ((2 0) . 467)
  ((6 0) . 114)
  ((2 2) . 35)
  ((7 5) . 58)
  ((1 0) . 467)
  ((7 2) . 633)
  ((0 0) . 467)
  ((6 2) . 633)
  ((5 9) . 598)
  ((3 6) . 592)
  ((2 6) . 592)
  ((0 4) . 617)
  ((8 7) . 755)
  ((7 0) . 114)
  ((2 9) . 664)
  ((7 9) . 598))


;; (map convert-to-hashmap (map find-numbers-and-coordinates schematic (stream->list (in-range (length schematic)))))


(#hash(((6 0) . 114) ((2 0) . 467) ((0 0) . 467) ((1 0) . 467) ((7 0) . 114) ((5 0) . 114))
 #hash()
 #hash(((8 2) . 633) ((3 2) . 35) ((7 2) . 633) ((2 2) . 35) ((6 2) . 633))
 #hash()
 #hash(((0 4) . 617) ((1 4) . 617) ((2 4) . 617))
 #hash(((8 5) . 58) ((7 5) . 58))
 #hash(((4 6) . 592) ((2 6) . 592) ((3 6) . 592))
 #hash(((7 7) . 755) ((8 7) . 755) ((6 7) . 755))
 #hash()
 #hash(((6 9) . 598) ((3 9) . 664) ((1 9) . 664) ((5 9) . 598) ((7 9) . 598) ((2 9) . 664)))

'("467..114.."
  "...*......"
  "..35..633."
  "......#..."
  "617*......"
  ".....+.58."
  "..592....."
  "......755."
  "...$.*...."
  ".664.598..")

#|

idea
keep hsh tables split out
iterate through every hash table
summate all the values found that match

|#


;; (foldl + 0 (flatten (map
;;  (λ (row) (remove-duplicates (flatten (map cdr row))))
;;  (group-by
;;   car
;;   (for*/list ([symbol-coord symbol-indices]
;;               [num-coord num-indices]
;;               #:when (are-coordinates-adjacent? symbol-coord num-coord))
;;     (list symbol-coord (hash-ref coord->number num-coord)))))))


(apply + (remove-duplicates (flatten (map
 (λ (row)
   (flatten (map cdr row)))
 (group-by
  car
  (for*/list ([symbol-coord symbol-indices]
              [num-coord num-indices]
              #:when (are-coordinates-adjacent? symbol-coord num-coord))
    (list symbol-coord (hash-ref coord->number num-coord))))))))


(apply + (flatten (map cdr
 (for*/list ([symbol-coord symbol-indices]
            [num-coord num-indices]
            #:when (are-coordinates-adjacent? symbol-coord num-coord))
  (list symbol-coord (hash-ref coord->number num-coord))))))


'((((3 1) 467) ((3 1) 35) ((3 1) 35))
  (((6 3) 633) ((6 3) 633))
  (((3 4) 617))
  (((5 5) 592))
  (((5 8) 755) ((5 8) 598) ((5 8) 598))
  (((3 8) 664) ((3 8) 664)))

'((467 35 35) (633 633) (617) (592) (755 598 598) (664 664))

'(((3 1) 467)
  ((3 1) 35)
  ((3 1) 35)
  ((6 3) 633)
  ((6 3) 633)
  ((3 4) 617)
  ((5 5) 592)
  ((5 8) 755)
  ((5 8) 598)
  ((5 8) 598)
  ((3 8) 664)
  ((3 8) 664))


(apply + (remove-duplicates
 (flatten (map
 cdr
 (for*/list ([symbol-coord symbol-indices]
              [num-coord num-indices]
              #:when (are-coordinates-adjacent? symbol-coord num-coord))
    (list symbol-coord (hash-ref coord->number num-coord)))
))))

;; (((3 1) 467)
;;  ((3 1) 35)
;;  ((3 1) 35)
;;  ((6 3) 633)
;;  ((6 3) 633)
;;  ((3 4) 617)
;;  ((5 5) 592)
;;  ((5 8) 755)
;;  ((5 8) 598)
;;  ((5 8) 598)
;;  ((3 8) 664)
;;  ((3 8) 664))

;; '(((1 9) . 664)
;;   ((8 5) . 58)
;;   ((3 2) . 35)
;;   ((6 7) . 755)
;;   ((5 0) . 114)
;;   ((2 4) . 617)
;;   ((3 9) . 664)
;;   ((6 9) . 598)
;;   ((4 6) . 592)
;;   ((7 7) . 755)
;;   ((8 2) . 633)
;;   ((1 4) . 617)
;;   ((2 0) . 467)
;;   ((6 0) . 114)
;;   ((2 2) . 35)
;;   ((7 5) . 58)
;;   ((1 0) . 467)
;;   ((7 2) . 633)
;;   ((0 0) . 467)
;;   ((6 2) . 633)
;;   ((5 9) . 598)
;;   ((3 6) . 592)
;;   ((2 6) . 592)
;;   ((0 4) . 617)
;;   ((8 7) . 755)
;;   ((7 0) . 114)
;;   ((2 9) . 664)
;;   ((7 9) . 598))
