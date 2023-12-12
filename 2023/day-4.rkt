#lang racket
(require racket advent-of-code data/monad data/applicative megaparsack megaparsack/text)

(define card/p
  (let ([space+ (many/p space/p)])
    (do (string/p "Card")
        space+
      [n <- integer/p]
      (string/p ":")
      space+
      [card-numbers <- (many-until/p integer/p #:sep (many/p space/p) #:end (try/p (string/p " | ")))]
      space+
      [your-numbers <- (many/p integer/p #:sep (many/p space/p))]
      (pure (cons n (list (first card-numbers) your-numbers))))))

(define scratch-cards (string-split (fetch-aoc-input (find-session) 2023 4) "\n"))
(define parsed-cards (map (compose parse-result! (curry parse-string card/p)) scratch-cards))

;; part 1
(define calc-points (λ (winners) (ceiling (expt 2 (sub1 (length winners))))))
(define original-scratch (map (λ (card) (set-intersect (cadr card) (last card))) parsed-cards))
(apply + (map calc-points (filter (compose not empty?) original-scratch)))

;; part 2

;; make alist with (digit . length-of-cards)
(define inter (map (λ (a b) (cons a (length b)))
                   (stream->list (in-inclusive-range 1 (length original-scratch))) original-scratch))

;; then iterate through
(let loop ([vals inter]
           [acc '()]
           [num-cards 0])
  (match vals
    ['() num-cards]
    [_ (match-let ([(cons digit next-counts) (first vals)])
         (let ([new-digits (stream->list (in-range 1 (add1 next-counts)))]
               [num-existing (add1 (length (indexes-of (flatten acc) digit)))])
           (loop (rest vals)
                 (cons
                  (make-list num-existing (map (curry + digit) new-digits))
                  acc)
                 (+ num-cards num-existing))))]))
