#lang racket
(require racket advent-of-code)

(define schematic (fetch-aoc-input (find-session) 2023 3))

(define schematic
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


(string-split schematic "\n")


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

;; Testing the function
(define symbol-indices
  (matrix-indexes-of
    (map string->list '("467..114.." "...*......" "..35..633." "......#..."
                        "617*......" ".....+.58." "..592....." "......755."
                        "...$.*...." ".664.598.."))
    not-alpha-numeric-or-dot?))

(define num-indices
  (matrix-indexes-of
    (map string->list '("467..114.." "...*......" "..35..633." "......#..."
                        "617*......" ".....+.58." "..592....." "......755."
                        "...$.*...." ".664.598.."))
    char-numeric?))



;; different tactic
;; get the coordinates of every number
;; write a function that checks if any of them are adjacent to a symbol

(define (are-coordinates-adjacent? coord1 coord2)
  (let* ([x1 (car coord1)]
         [y1 (cadr coord1)]
         [x2 (car coord2)]
         [y2 (cadr coord2)]
         [dx (abs (- x1 x2))]
         [dy (abs (- y1 y2))])
    (and (<= dx 1) (<= dy 1))))


(map
 (λ)
 (group-by
 car
 (for*/list ([symbol-coord symbol-indices]
             [num-coord num-indices]
             #:when (are-coordinates-adjacent? symbol-coord num-coord))
   (list symbol-coord num-coord))))

'((((3 1) (2 0)) ((3 1) (3 2)) ((3 1) (2 2)))
  (((6 3) (7 2)) ((6 3) (6 2)))
  (((3 4) (2 4)))
  (((5 5) (4 6)))
  (((5 8) (6 7)) ((5 8) (6 9)) ((5 8) (5 9)))
  (((3 8) (3 9)) ((3 8) (2 9))))


(((3 1) (2 0)) ((3 1) (3 2)) ((3 1) (2 2)))
(((3 1) (2 0)) ((3 1) (3 2)) ((3 1) (2 2)))


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


(define (matrix-ref lst x y)
  (string-ref (list-ref lst y) x))

#|

idea
given a coordinate, take characters from the left and right as long as they satisfy a predicate

|#


(let ([exstr "467..114.."]
      [excoord '(2 0)])
  (string-ref exstr (first excoord)))
