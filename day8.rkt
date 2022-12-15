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

(define (split-trio ls pos)
  (let* ([part (values->list (split-at ls pos))]
         [before (first part)]
         [val (caadr part)]
         [after (rest (cadr part))])
    (list before val after)))

;; now put it together
(define (check-cross-section trio)
  (let* ([base-tree (second trio)]
         [side1 (first trio)]
         [side2 (last trio)])
    (or (andmap (λ (t) (> base-tree t)) side1)
        (andmap (λ (t) (> base-tree t)) side2))))

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
        (visible? x y tree-heights))))

;; pt 1
(+ (count identity visibility)
   (* 4 (sub1 (first (vector->list (array-shape tree-heights))))))
