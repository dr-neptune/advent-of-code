#lang racket
(require racket advent-of-code megaparsack megaparsack/text data/monad data/applicative)

(define rocks (fetch-aoc-input (find-session) 2022 14))

(define exrocks
  #<<"
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"
  )

;; idea
;; these determine paths
;; so we should get the ranges betweeen then, i.e. the differences between each x,y pair
(define x-y/p
  (do [x <- integer/p]
      [_ <- (char/p #\,)]
    [y <- integer/p]
    (pure (list x y))))

(define arrow/p
  (do [_ <- (string/p " -> ")]
      (pure '())))

(define many-x-y/p
  (many/p (or/p x-y/p arrow/p)))

(define x-y
  (map
   (λ (in-str)
     (filter (λ (v) (not (empty? v)))
             (parse-result! (parse-string many-x-y/p in-str))))
   (string-split exrocks "\n")))
