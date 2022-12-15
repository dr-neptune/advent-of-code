#lang racket
(require racket math/array threading advent-of-code racket/pretty (only-in br/list values->list))

(define trees (fetch-aoc-input (find-session) 2022 8))

(define tree-heights
  (~>> trees
       string-split
       (map (compose list->array (λ (c) (map char->integer c)) string->list))
       array-list->array))

;; split a list into '((before) pos (after))
(define (split-trio ls pos)
  (let* ([part (values->list (split-at ls pos))]
         [before (first part)]
         [val (caadr part)]
         [after (rest (cadr part))])
    (list before val after)))

(define (check-visibility-cross-section trio)
  (let* ([base-tree (second trio)]
         [side1 (first trio)]
         [side2 (last trio)])
    (or (andmap (λ (t) (> base-tree t)) side1)
        (andmap (λ (t) (> base-tree t)) side2))))

(define (visible? r c 2d-array)
  (let ([up-down-split (split-trio (array->list (array-slice-ref 2d-array (list (::) (list c)))) r)]
        [left-right-split (split-trio (array->list (array-slice-ref 2d-array (list (list r) (::)))) c)])
    (or (check-visibility-cross-section up-down-split)
        (check-visibility-cross-section left-right-split))))

(define visibility-map
  (let* ([arr-shape (vector->list (array-shape tree-heights))]
         [arr-height (first arr-shape)]
         [arr-width (second arr-shape)])
    (for*/list ([x (in-range 1 (sub1 arr-height))]
                [y (in-range 1 (sub1 arr-width))])
        (visible? x y tree-heights))))

;; pt 1
(+ (count identity visibility-map)
   (* 4 (sub1 (vector-ref (array-shape tree-heights) 0))))

;; pt 2
(define (count-trees stop-height tree-heights)
  (cond [(>= (first tree-heights) stop-height) 1]
        [(equal? (length tree-heights) 1) 1]
        [else (add1 (count-trees stop-height (rest tree-heights)))]))

(define (get-tree-score trio)
  (apply * (map ((curry count-trees) (second trio))
                (list (reverse (first trio)) (last trio)))))

(define (get-tree-visibilities r c 2d-array)
  (let ([up-down-split (split-trio (array->list (array-slice-ref 2d-array (list (::) (list c)))) r)]
        [left-right-split (split-trio (array->list (array-slice-ref 2d-array (list (list r) (::)))) c)])
    (* (get-tree-score up-down-split)
       (get-tree-score left-right-split))))

;; now we want to apply it to the inner square
(define tree-visibility-scores
  (let ([arr-shape (vector->list (array-shape tree-heights))])
    (for*/list ([x (in-range 1 (sub1 (first arr-shape)))]
                [y (in-range 1 (sub1 (second arr-shape)))])
      (get-tree-visibilities x y tree-heights))))

(apply max tree-visibility-scores)
