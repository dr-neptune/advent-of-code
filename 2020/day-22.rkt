#lang racket
(require racket threading advent-of-code)

(define deck
  (~> (fetch-aoc-input (find-session) 2020 22)
      (string-split "\n\n")
      (map (λ~>
            (string-split ":")
            ((λ (pair)
               (list (first pair)
                     (map string->number (string-split (second pair) "\n")))))) _)))

(define (play-game deck)
  (match-let ([(list (list p1n p1-hand) (list p2n p2-hand)) deck])
    (let loop ([p1 p1-hand] [p2 p2-hand])
      (cond [(empty? p1) (list p2n p2)]
            [(empty? p2) (list p1n p1)]
            [(> (first p1) (first p2))
             (loop (append (rest p1) (list (first p1) (first p2))) (rest p2))]
            [else (loop (rest p1)
                        (append (rest p2) (list (first p2) (first p1))))]))))

(define (deck-key p1 p2)
  (string-append
   (string-join (map number->string p1) ",") "|" (string-join (map number->string p2) ",")))

(define (play-recursive-game deck)
  (match-let ([(list (list p1n p1) (list p2n p2)) deck]
              [memo (make-hash)])
    (let loop ([p1 p1] [p2 p2])
      (let ([key (deck-key p1 p2)])
        (if (hash-has-key? memo key)
            ;; Previous state encountered, player 1 wins
            (list "Player 1" p1)
            (begin
              (hash-set! memo key #t)
              (cond
                [(empty? p1) (list "Player 2" p2)]
                [(empty? p2) (list "Player 1" p1)]
                [else
                 (match-let* ([(list c1 rest-p1 ...) p1]
                              [(list c2 rest-p2 ...) p2]
                              [winner
                               (cond [(and (>= (length rest-p1) c1)
                                           (>= (length rest-p2) c2))
                                      ;; Recurse into a sub-game
                                      (first (play-recursive-game
                                              (list (list "Player 1" (take rest-p1 c1))
                                                    (list "Player 2" (take rest-p2 c2)))))]
                                     [(> c1 c2) "Player 1"]
                                     [else "Player 2"])])
                   (if (string=? winner "Player 1")
                       (loop (append rest-p1 (list c1 c2)) rest-p2)
                       (loop rest-p1 (append rest-p2 (list c2 c1)))))])))))))

(define (get-score deck game)
  (match-let ([(list _ cards) (game deck)])
    (for/sum ([card cards]
              [idx (in-inclusive-range (length cards) 0 -1)])
      (* card idx))))

;; part 1
(get-score deck play-game)

;; part 2
(get-score deck play-recursive-game)
