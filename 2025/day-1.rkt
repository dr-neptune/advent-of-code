#lang racket
(require racket advent-of-code threading
         (only-in br/list values->list))

(define input (fetch-aoc-input (find-session) 2025 1))

(define input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(define rotation-doc
  (for/list ([dial (in-list (regexp-match* #px"([LR])(\\d+)" input #:match-select values))])
    (match-let* ([(list _ dir amt) dial] [amt (string->number amt)])
      (if (string=? dir "L") (- amt) amt))))

;; part 1
(for/fold ([num 50] [zeros 0])
          ([turn rotation-doc])
  (let ([curr (modulo (+ num turn) 100)])
    (if (zero? curr)
        (values curr (add1 zeros))
        (values curr zeros))))

;; part 2
(define (count-zeros num turn)
  (let* ([rng (if (negative? turn) (in-range 0 turn -1) (in-range turn))])
    (for/sum ([offset rng]
              #:do [(define curr (modulo (+ num offset) 100))]
              #:when (zero? curr))
      1)))

(for/fold ([num 50] [zeros 0])
          ([turn rotation-doc])
  (let ([curr (modulo (+ num turn) 100)])
    (values curr (+ zeros (count-zeros num turn)))))
