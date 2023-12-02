#lang racket
(require racket advent-of-code threading)

(define datastream-buffer (fetch-aoc-input (find-session) 2022 6))

(define (sliding-window ls n [vals '()])
  (if (= n (length ls))
      (reverse (cons ls vals))
      (sliding-window (rest ls) n (cons (take ls n) vals))))

(define (get-start-of-message datastream message-size)
  (~> datastream
      string->list
      (sliding-window _ message-size)
      (map check-duplicates _)
      (index-of #f)
      (+ _ message-size)))

;; pt1
(get-start-of-message datastream-buffer 4)

;; pt2
(get-start-of-message datastream-buffer 14)
