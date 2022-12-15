#lang racket
(require racket math/array threading advent-of-code racket/pretty (only-in br/list values->list))

;; get data
(define trees (fetch-aoc-input (find-session) 2022 8))

(define tree-heights
  (~>> trees
       string-split
       (map (compose list->array (λ (c) (map char->integer c)) string->list))
       array-list->array))

;; split list into 3 at pos. ex ((1 2 3 4 5) 2) -> ((1 2) 3 (4 5))
(define (split-trio ls pos)
  (let-values ([(before after) (split-at ls pos)])
    (list (reverse before) (first after) (rest after))))

;; make score fn
(define (get-score trio score-fn)
  (let* ([base-tree (second trio)]
         [side1 (first trio)]
         [side2 (last trio)])
    (score-fn base-tree side1 side2)))

;; split horizontal and vertical
(define (get-splits r c 2d-array)
  (let ([up-down-split (split-trio (array->list (array-slice-ref 2d-array (list (::) (list c)))) r)]
        [left-right-split (split-trio (array->list (array-slice-ref 2d-array (list (list r) (::)))) c)])
    (values up-down-split left-right-split)))

;; apply scores to the inner square of trees
(define (apply-scores score-fn array)
  (let ([arr-shape (vector->list (array-shape array))])
    (for*/list ([x (in-range 1 (sub1 (first arr-shape)))]
                [y (in-range 1 (sub1 (second arr-shape)))])
      (score-fn x y array))))

;; scoring function
(define (check-visibility-cross-section trio)
  (get-score trio (λ (base-tree side1 side2)
                    (or (andmap (λ (t) (> base-tree t)) side1)
                        (andmap (λ (t) (> base-tree t)) side2)))))

;; split apply function
(define (visible? r c 2d-array)
  (let-values ([(up-down-split left-right-split) (get-splits r c 2d-array)])
    (ormap check-visibility-cross-section (list up-down-split left-right-split))))

;; application to square
(define visibility-map (apply-scores visible? tree-heights))

;; pt 1
(+ (count identity visibility-map)
   (* 4 (sub1 (vector-ref (array-shape tree-heights) 0))))

;; pt 2
;; scoring function
(define (count-trees stop-height tree-heights)
  (cond [(>= (first tree-heights) stop-height) 1]
        [(equal? (length tree-heights) 1) 1]
        [else (add1 (count-trees stop-height (rest tree-heights)))]))

(define (get-tree-score trio)
  (get-score trio (λ (base-tree side1 side2)
                    (apply * (map (curry count-trees base-tree) (list side1 side2))))))

;; split apply function
(define (get-tree-visibilities r c 2d-array)
  (let-values ([(up-down-split left-right-split) (get-splits r c 2d-array)])
    (* (get-tree-score up-down-split)
       (get-tree-score left-right-split))))

;; application to inner square
(define tree-visibility-scores (apply-scores get-tree-visibilities tree-heights))

;; result
(apply max tree-visibility-scores)
