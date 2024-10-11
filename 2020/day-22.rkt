#lang racket
(require racket threading advent-of-code)


(define deck
  (~>
;; "Player 1:
;; 9
;; 2
;; 6
;; 3
;; 1

;; Player 2:
;; 5
;; 8
;; 4
;; 7
;; 10"
 (fetch-aoc-input (find-session) 2020 22)
  (string-split "\n\n")
    (map (λ~>
          (string-split ":")
          ((λ (pair)
            (list (first pair)
                  (map string->number (string-split (second pair) "\n")))))) _)))

;; let loop?
(define (play-game deck)
  (match-let ([(list (list p1n p1-hand) (list p2n p2-hand)) deck])
    (let loop ([p1 p1-hand] [p2 p2-hand])
      (cond [(empty? p1) (list p2n p2)]
            [(empty? p2) (list p1n p1)]
            [(> (first p1) (first p2))
             (loop (append (rest p1) (list (first p1) (first p2))) (rest p2))]
            [else (loop (rest p1)
                        (append (rest p2) (list (first p2) (first p1))))]))))

(match-let ([(list _ results) (play-game deck)])
  (for/sum ([card results]
            [idx (in-inclusive-range (length results) 0 -1)])
    (* card idx)))

;; part 2
#|

if a loop, the game instantly ends in a win for p1

draw a card
if both players have AT LEAST as many cards remaining in their deck as the value of the card they drew,
the winner of the round is determined by playing another game of recursive combat

otherwise the winner of the round is the player with the higher value card

subgame rules:
make a copy of the next cards in their deck (number on the card drawn to trigger the subgame)

|#

;; need to add memoization

(define (play-recursive-game deck)
  (let ([memo (make-hash)])
    (match-let ([(list (list p1n p1-hand) (list p2n p2-hand)) deck])
      (let loop ([p1 p1-hand] [p2 p2-hand])
        (if (hash-has-key? memo (append p1 p2))
            (begin
              ;; (displayln (format "loop found! p1: ~a p2: ~a" p1 p2))
              "Player 1")
            (begin
              ;; (displayln (format "adding to hash: ~a\n\ncurr: ~a" (append p1 p2) memo))
              (hash-set! memo (append p1 p2) 1)))
        ;; (displayln (format "~a ~a" p1 p2))
        (cond [(empty? p1) (list p2n p2)]
              [(empty? p2) (list p1n p1)]
              [(and (>= (length (rest p1)) (first p1))
                    (>= (length (rest p2)) (first p2)))
               (begin
                 ;; (displayln (format "looping!"))
                 (match-let ([(list winner subgame-result)
                              (play-recursive-game (list (list p1n (take (rest p1) (first p1)))
                                                         (list p2n (take (rest p2) (first p2)))))])
                   (begin
                    ;;  (displayln (format "winner! ~a" winner))
                     (match winner
                       [(== p1n) (loop (append (rest p1) (list (first p1) (first p2))) (rest p2))]
                       [_ (loop (rest p1)
                                (append (rest p2) (list (first p2) (first p1))))]))))]
              [(> (first p1) (first p2))
               (loop (append (rest p1) (list (first p1) (first p2))) (rest p2))]
              [else (loop (rest p1)
                          (append (rest p2) (list (first p2) (first p1))))])))))

;; (match-let ([(list _ results) (play-recursive-game deck)])
;;   (for/sum ([card results]
;;             [idx (in-inclusive-range (length results) 0 -1)])
;;     (* card idx)))

;; not converging yet!
