#lang racket
(require racket advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 1))

(define rotation-doc
  (for/list ([dial (in-list (regexp-match* #px"([LR])(\\d+)" input #:match-select values))])
    (match-let* ([(list _ dir amt) dial] [amt (string->number amt)])
      (if (string=? dir "L") (- amt) amt))))

;; part 1: count landings on zero
(for/fold ([num 50] [zeros 0])
          ([turn rotation-doc])
  (let ([curr (modulo (+ num turn) 100)])
    (values curr (+ zeros (if (zero? curr) 1 0)))))

;; part 2: also count passes over zero mid-rotation
(define (count-zeros num turn)
  (let ([step (if (negative? turn) -1 1)])
    (for/sum ([offset (in-inclusive-range step turn step)]
              #:when (zero? (modulo (+ num offset) 100)))
      1)))

(for/fold ([num 50] [zeros 0])
          ([turn rotation-doc])
  (let ([curr (modulo (+ num turn) 100)])
    (values curr (+ zeros (count-zeros num turn)))))
