#lang racket
(require racket advent-of-code threading megaparsack megaparsack/text data/monad data/applicative)

;; parsing input
;; separate instructions from supplies
(match-define (list supplies instructions)
  (~>> (fetch-aoc-input (find-session) 2022 5)
       (string-split _ "\n\n")
       (map (λ (s) (string-split s "\n")))))

;; use a parser to parse each supply
(define triple/p
  (do [x <- (or/p space/p (char/p #\[))]
      [y <- (or/p letter/p space/p)]
    [z <- (or/p space/p (char/p #\]))]
    (pure y)))

(define (parse-supply-line supply-line)
  (parse-result! (parse-string (many/p triple/p #:sep space/p) supply-line)))

;; parse each list and transpose to get rows
(define (transpose-list-of-lists lol)
  (apply map list lol))

(define (get-supplies-rows supplies)
  (~>> supplies
       (map parse-supply-line)
       transpose-list-of-lists
       (map
        (λ (supply-chain) (remove* '(#\space) supply-chain)))))

(define supplies (get-supplies-rows (drop-right supplies 1)))

;; parse instructions
(define instruction/p
  (many/p
   (do (many/p (or/p (char-between/p #\a #\z) space/p))
       integer/p)))

;; "move x from y to z" -> '(x y z)
(define (parse-instruction instruction)
  (parse-result! (parse-string instruction/p instruction)))

(define instructions (map parse-instruction instructions))

;; problem
;; keep amount, adjust location for 0-indexing
(define (update-instructions instructions)
  (map (λ (3tup) (cons (first 3tup) (map sub1 (rest 3tup)))) instructions))

;; move an amount of boxes from starting-loc to ending-loc, return updated configuration
(define (move-box supplies amount starting-location ending-location)
    (list-update
     (list-update
      supplies
      ending-location
      (λ _
        (append
         (take (list-ref supplies starting-location) amount)
         (list-ref supplies ending-location))))
     starting-location
     (λ (ls) (drop ls amount))))

;; move the crates one at a time
(define (move amount from to supplies)
  (let movement ([amt amount]
                 [cargo supplies])
    (cond [(zero? amt) cargo]
          [(movement (sub1 amt) (move-box cargo 1 from to))])))

;; pt 1
(map first (foldl (λ (i s) ((apply (curry move) i) s)) supplies (update-instructions instructions)))

;; pt 2
(map first (foldl (λ (i s) (apply ((curry move-box) s) i)) supplies (update-instructions instructions)))
