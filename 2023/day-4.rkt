#lang racket
(require racket advent-of-code data/monad data/applicative megaparsack megaparsack/text)

(define scratch-cards (string-split (fetch-aoc-input (find-session) 2023 4) "\n"))

(define scratch-cards
  (string-split
   "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
   "\n"))

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

(define parsed-cards (map (compose parse-result! (curry parse-string card/p)) scratch-cards))
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
