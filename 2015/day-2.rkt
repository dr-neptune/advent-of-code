#lang racket
(require racket threading advent-of-code)

(define dims (~> (fetch-aoc-input (find-session) 2015 2)
                 (string-split "\n")
                 (map (compose
                       (curry map string->number)
                       (curry regexp-match* #rx"([0-9]+|[0-9]+|[0-9]+)")) _)))

(define (get-sqft-wrapping-paper dim)
  (match-let ([(list l w h) dim])
    (+ (+ (* 2 l w)
          (* 2 w h)
          (* 2 h l))
       (apply * (take (sort dim <) 2)))))

(apply + (map get-sqft-wrapping-paper dims))

;; part 2
(define (get-ribbon-amt dim)
  (let ([sorted-dims (sort dim <)])
    (+ (apply * dim)
       (+ (* 2 (first sorted-dims))
          (* 2 (second sorted-dims))))))

(apply + (map get-ribbon-amt dims))
