#lang racket
(require racket advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

(define boat-races (string-split (fetch-aoc-input (find-session) 2023 6) "\n"))

(define boat-races (string-split "Time:      7  15   30
Distance:  9  40  200" "\n"))

(define boat-races/p
  (let ([space+ (many/p space/p)])
    (do (many/p (char-between/p #\A #\z))
        space+
      (string/p ":")
      space+
      [boat-ints <- (many/p integer/p #:sep (many/p space/p))]
      (pure boat-ints))))

(define time-dists
  (let ([parsed (map (compose parse-result! (curry parse-string boat-races/p)) boat-races)])
    (map cons (first parsed) (second parsed))))


#|

start with 7 9

hold for 0, go 0
hold for 1, then go for 6 seconds at 1 / s for 6
hold for 2, then go for 5 seconds at 2 / s for 10
hold for 3, then go for 4 seconds at 3 / 2 for 12

f(x) = (7 - x) * x
     = 7x - x^2
|#

;; part 1
(let* ([move-fn (λ (v t) (* v (- t v)))]
       [check-fn (λ (t dist)
                   (stream->list
                    (stream-filter (curry < dist)
                                   (stream-map (curryr move-fn t) (in-range 0 t)))))])
  (apply * (map (λ (td) (length (check-fn (car td) (cdr td)))) time-dists)))


;; part 2
(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

(define concat-time-dists
  (let ([concat-digit-list (λ (digit-list) (digit-list->int (flatten (map int->digit-list digit-list))))])
    (cons (concat-digit-list (map car time-dists))
          (concat-digit-list (map cdr time-dists)))))

(let* ([move-fn (λ (v t) (* v (- t v)))]
       [check-fn (λ (t dist)
                   (stream->list
                    (stream-filter (curry < dist)
                                   (stream-map (curryr move-fn t) (in-range 0 t)))))])
  (apply * (map (λ (td) (length (check-fn (car td) (cdr td)))) (list concat-time-dists))))
