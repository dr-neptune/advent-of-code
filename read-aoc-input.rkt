#lang racket
(require racket)

(provide read-aoc-input)

(define (read-aoc-input day)
  (with-output-to-string
    (lambda () (system (format "raco aoc -y 2022 -d ~a" day)))))
