#lang racket
(require racket advent-of-code
         data/applicative
         data/monad
         megaparsack
         megaparsack/text)

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

(apply +
       (map calc-points
            (filter (compose not empty?)
                    (map (λ (card) (set-intersect (cadr card) (last card))) parsed-cards))))

;; part 2
(define original-num-scratchcards (length scratch-cards))

(define original-scratch (map (λ (card) (set-intersect (cadr card) (last card))) parsed-cards))

#|

keep original-draw separate
for card 1
- get number of winners (4)
- then take next 4 cards and append them to the rest of the cards
  - re-sort
|#

(define hsh-scratch
  (for/hash ([idx (in-inclusive-range 1 (length original-scratch))]
             [scratch original-scratch])
    (values idx scratch)))

'((1 . (17 86 83 48)) (2 . (61 32)) (3 . (21 1)) (4 . (84)) (5 . ()) (6 . ()))

(define acc (stream->list (in-inclusive-range 1 (hash-count hsh-scratch))))

#|

make a hash map
'#hash((1 . (17 86 83 48)) (2 . (61 32)) (3 . (21 1)) (4 . (84)) (5 . ()) (6 . ()))

make an accumulator, add cards 1 -> 6
'(1 2 3 4 5 6)

1 gets next 4 cards
add to accumulator

|#

;; (let ([acc (stream->list (in-inclusive-range 1 (hash-count hsh-scratch)))])
;;   (hash-map hsh-scratch
;;   (lambda (key value)
;;     (printf "key: ~a, value: ~a\n" key value)
;;     (let ([num-to-take (length value)])
;;       (set! acc (sort (flatten (cons (stream->list (in-inclusive-range (add1 key) (+ key num-to-take))) acc)) <)))))
;;  acc)

;; hsh-scratch
'((1 . (17 86 83 48)) (2 . (61 32)) (3 . (21 1)) (4 . (84)) (5 . ()) (6 . ()))

#|
'(1 2 3 4 5 6)
1. num wins: 4

'(1 2 2 3 3 4 4 5 5 6)

2. num wins: 2
'(1 2 2 3 3 3 3 4 4 4 4 5 5 6)

3. num wins: 2
'(1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 6)

4. num wins: 1
'(1 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6)

5. num wins: 0

6. num wins: 0

keep track of current counts of each number
iterate through the keys and get the number of winners
for curr count times, add each of the following numbers

|#

(define hsh-counter
  (for/hash ([idx (in-inclusive-range 1 (length original-scratch))])
    (values idx 1)))


(define hsh-scratch
  (for/hash ([idx (in-inclusive-range 1 (length original-scratch))]
             [scratch original-scratch])
    (values idx scratch)))

(define hsh-counter (make-hash))

(hash-map hsh-scratch
  (lambda (key value)
    ;; (printf "key: ~a, value: ~a\n" key value)
    (let ([num-to-take (length value)]
          [num-tickets (hash-ref hsh-counter key 1)])
      (let ([tickets-to-add (stream->list (in-inclusive-range (add1 key) (+ key num-to-take)))])
        (for ([ticket tickets-to-add])
          (displayln hsh-counter)
          (hash-update! hsh-counter key (λ (v) (+ v ticket)) 1))
        (displayln (format "nt: ~a num take: ~a to add: ~a" num-tickets num-to-take tickets-to-add)))
      )))

(map (curry * 2) '(1 2 3))
