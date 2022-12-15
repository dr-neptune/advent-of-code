#lang racket
(require racket math/array racket/pretty (only-in br/list values->list))

(define extrees
  #<<"
30373
25512
65332
33549
35390
"
)

(define trees (fetch-aoc-input (find-session) 2022 8))

;; idea
;; grab all digits, place in list and convert to array
(define exarray
  (~>> extrees
       string-split
       (map (compose list->array (λ (c) (map char->integer c)) string->list))
       array-list->array))

(define tree-heights
  (~>> trees
       string-split
       (map (compose list->array (λ (c) (map char->integer c)) string->list))
       array-list->array))


;; look around plus sign
(pretty-print (array-ref exarray #(2 3)))

(map ((curry array-ref) exarray) '(#(1 2) #(2 1) #(2 2) #(2 3) #(3 2)))

(map (λ (coord label) (cons label (array-ref exarray coord)))
     '(#(1 2) #(2 1) #(2 2) #(2 3) #(3 2))
     '("up" "left" "base" "right" "down"))


(define (get-surrounding-trees x y 2d-array)
  (map (λ (coord label) (cons label (array-ref 2d-array coord)))
       (list (vector (sub1 x) y)
             (vector x (sub1 y))
             (vector x y)
             (vector x (add1 y))
             (vector (add1 x) y))
       '("up" "left" "base" "right" "down")))

(pretty-display exarray)

(define (visible? x y 2d-array)
  (let ([sorted-tree-heights
         (sort (get-surrounding-trees x y 2d-array) (λ (a b) (< (cdr a) (cdr b))))])
    (or (equal? (caar sorted-tree-heights) "base")
        (and (equal? (caadr sorted-tree-heights) "base")
             (equal? (cdar sorted-tree-heights)
                     (cdadr sorted-tree-heights))))))

(define validity
  (let* ([arr-shape (vector->list (array-shape exarray))]
         [arr-height (first arr-shape)]
         [arr-width (second arr-shape)])
    (for*/list ([x (in-range 1 (sub1 arr-height))]
                [y (in-range 1 (sub1 arr-width))])
      (begin
        (println (format "x: ~a y: ~a" x y))
        (visible? x y exarray)))))

(count identity validity)

'(#f #f #t
  #f #t #f
  #t #f #f)

;; we only care about those values which are
;; close to the edge

;; redo the validity function so that it takes all the values to the edge
;; then if all the other values in the slice are less than the given value, it is visible
(pretty-display exarray)
(array-slice-ref exarray (list '(0 1) '(0 1 2)))

(array-slice-ref exarray (list '(2 2) '(0 1 2 3 4)))
(array-slice-ref exarray (list (::) '(0)))

(array->list (array-slice-ref exarray (list '(2) (::))))
(array->list (array-slice-ref exarray (list (::) '(2))))

(caadr (values->list (split-at (array->list (array-slice-ref exarray (list '(2) (::)))) 2)))

(define (split-trio ls pos)
  (let* ([part (values->list (split-at ls pos))]
         [before (first part)]
         [val (caadr part)]
         [after (rest (cadr part))])
    (list before val after)))

(split-trio (array->list (array-slice-ref exarray (list '(2) (::)))) 1)


;; we have the up downs and across
;; now we need to split the values

;; ok, so check if all the values before and after are shorter


(array->list (array-slice-ref exarray (list '(1) (::))))
(array->list (array-slice-ref exarray (list (::) '(2))))

(let* ([trio (split-trio (array->list (array-slice-ref exarray (list '(2) (::)))) 2)]
       [base-tree (second trio)]
       [above (first trio)]
       [below (last trio)])
  (list (map (λ (t) (> base-tree t)) above)
        (map (λ (t) (> base-tree t)) below)))


(let* ([trio (split-trio (array->list (array-slice-ref exarray (list '(2) (::)))) 3)]
       [base-tree (second trio)]
       [above (first trio)]
       [below (last trio)])
  (or (andmap (λ (t) (> base-tree t)) above)
      (andmap (λ (t) (> base-tree t)) below)))


(let* ([trio (split-trio (array->list (array-slice-ref exarray (list (::) '(2)))) 3)]
       [base-tree (second trio)]
       [above (first trio)]
       [below (last trio)])
  (list (andmap (λ (t) (> base-tree t)) above)
        (andmap (λ (t) (> base-tree t)) below)))

;; now put it together
(define (check-cross-section trio)
  (let* ([base-tree (second trio)]
         [side1 (first trio)]
         [side2 (last trio)])
    (or (andmap (λ (t) (> base-tree t)) side1)
        (andmap (λ (t) (> base-tree t)) side2))))



(check-cross-section (split-trio (array->list (array-slice-ref exarray (list (::) '(2)))) 3))

(define (visible? r c 2d-array)
  (let ([up-down-split (split-trio (array->list (array-slice-ref 2d-array (list (::) (list c)))) r)]
        [left-right-split (split-trio (array->list (array-slice-ref 2d-array (list (list r) (::)))) c)])
    (or (check-cross-section up-down-split)
        (check-cross-section left-right-split))))


(define visibility
  (let* ([arr-shape (vector->list (array-shape tree-heights))]
         [arr-height (first arr-shape)]
         [arr-width (second arr-shape)])
    (for*/list ([x (in-range 1 (sub1 arr-height))]
                [y (in-range 1 (sub1 arr-width))])
      (begin
        (println (format "x: ~a y: ~a" x y))
        (visible? x y tree-heights)))))

;; pt 1
(+ (count identity visibility)
   (apply * (map sub1 (vector->list (array-shape tree-heights)))))
