#lang racket
(require racket advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text
         (only-in srfi/1 unfold-right)
         (only-in srfi/26 cut))

;; data shaping
(define boat-races (string-split (fetch-aoc-input (find-session) 2023 6) "\n"))

(define (string-eval-parser str-in parser)
  (parse-result! (parse-string parser str-in)))

(define boat-races/p
  (let ([space+ (many/p space/p)])
    (do (many/p (char-between/p #\A #\z))
        space+
      (string/p ":")
      space+
      [boat-ints <- (many/p integer/p #:sep (many/p space/p))]
      (pure boat-ints))))

(define time-dists
  (let ([parsed (map (curryr string-eval-parser boat-races/p) boat-races)])
    (map cons (first parsed) (second parsed))))

(define (digit-list->int ls)
  (foldl (lambda (digit power) (+ (* 10 power) digit)) 0 ls))

(define (int->digit-list int)
  (unfold-right zero? (cut remainder <> 10) (cut quotient <> 10) int))

;; part 1
(define (winning-ways time-dists)
  (let* ([move-fn (λ (v t) (* v (- t v)))]
         [check-fn
          (λ (t dist)
            ((compose stream->list
                      (curry stream-filter (curry < dist))
                      (curry stream-map (curryr move-fn t)))
             (in-range t)))])
    (apply * (map (λ (td) (length (check-fn (car td) (cdr td)))) time-dists))))

(winning-ways time-dists)

;; part 2
(define concat-time-dists
  (let* ([digit-concat (compose digit-list->int flatten (curry map int->digit-list))]
         [concat-digit-list (λ (digit-list) (digit-concat digit-list))]
         [get-concat-num (λ (f) (concat-digit-list (map f time-dists)))])
    (cons (get-concat-num car) (get-concat-num cdr))))

(winning-ways (list concat-time-dists))

;; part 2 with math
;; f(x) = x(t - x) = xt - x^2
;; we want xt - x^2 > d -> xt - x^2 - d > 0 -> -x^2 + xt - d > 0
;; -b +- sqrt(b^2 - 4ac) / 2a
;; a = -1 b = t c = -d

(match-let ([(cons t d) concat-time-dists])
  (let ([discriminant (- (* t t) (* 4 d))])
    (exact-ceiling
     (- (/ (+ t (sqrt discriminant)) 2)
        (/ (- t (sqrt discriminant)) 2)))))
