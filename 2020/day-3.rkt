#lang racket
(require racket threading advent-of-code)

(define tree-grid (~> (fetch-aoc-input (find-session) 2020 3) (string-split "\n")))

(define (get-slope-count right down)
  (for/fold ([points '()]
             [curr-point right]
             [idx 0]
             #:result (count (curry equal? #\#) points))
            ([tree-line (drop tree-grid down)]
             #:do [(define trees (string->list tree-line))])
    (if (zero? (remainder idx down))
        (values (cons (list-ref trees curr-point) points)
                (remainder (+ curr-point right) (length trees))
                (add1 idx))
        (values points curr-point (add1 idx)))))

;; part 1
(get-slope-count 3 1)

;; part 2
(* (get-slope-count 1 1)
   (get-slope-count 3 1)
   (get-slope-count 5 1)
   (get-slope-count 7 1)
   (get-slope-count 1 2))


(let ([skip-iter 3])
  (for/list ([idx (in-naturals)]
             [ls-ele '(1 2 3 4 5 6)])
    (when (zero? (remainder idx skip-iter))
      ls-ele)))
