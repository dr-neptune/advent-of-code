#lang racket
(require racket threading advent-of-code)

(define (dprod a b)
  (let ([total (apply + (map * a b))])
    (if (negative? total)
        0
        total)))

(define (groupwise . args) args)

(define (generate-splits n total)
  (if (= n 1)
      (list (list total))
      (apply append
             (map (λ (i)
                    (map (λ (split) (cons i split))
                         (generate-splits (- n 1) (- total i))))
                  (range 1 (+ 1 total))))))

(define re-pattern #px"^(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)")

(define ingredients
  (~> (fetch-aoc-input (find-session) 2015 15)
      (string-split "\n")
      (map
       (λ (s) (match-let ([(list _ ingredient capacity durability flavor texture calories)
                           (regexp-match re-pattern s)])
                (cons ingredient (map string->number (list capacity durability flavor texture calories)))))
       _)))

(define (calculate-amounts . splits)
  (define (calculate-value capacities durabilities flavors textures calories)
    (map (curry dprod splits) (list capacities durabilities flavors textures calories)))
  calculate-value)

(define (calculate-values break-points)
  (apply (apply calculate-amounts break-points)
         (apply (curry map groupwise) (map rest ingredients))))


;; part 1
(define splits (generate-splits (length ingredients) 100))

(~>> (map (λ~> calculate-values
               (drop-right _ 1)
               (apply * _))
          splits)
     (apply max))

;; part 2
(~>> (map (λ~> (drop-right _ 1) (apply * _))
          (filter
           (λ~> last (equal? 500))
           (map calculate-values splits)))
     (apply max))
