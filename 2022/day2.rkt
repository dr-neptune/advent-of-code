#lang racket
(require racket threading advent-of-code racket/hash)

(define roshambo (fetch-aoc-input (find-session) 2022 2))

;; idea
;; There are 3 sets of rules to keep track of:
;;  1. the rules of rock paper scissors
;;    rules-gen makes 2 functions: rules and rules-rev
;;  2. what ABC XYZ mean
;;    translate-gen makes 2 functions: translate (pt 1) and translate-fixed (pt 2)
;;  3. how many points each hand is worth. Handled by hand-score

;; game-result calculates the outcome of a game
;; for part b, the fix function modifies the way choice is made in order to obey new rules

;; calculate-results parses the game ledger and provides the sum of the game scores

(define rules-gen
  (let* ([*rules* (hash
                   "rock" "scissors"
                   "scissors" "paper"
                   "paper" "rock")]
         [*reverse-rules* (hash-map/copy *rules* (λ (k v) (values v k)))])
    (list
     ((curry hash-ref) *rules*)
     ((curry hash-ref) *reverse-rules*))))

(match-define (list rules rules-rev) rules-gen)

(define translate-gen
  (let ([*translator-base* (hash
                            "A" "rock"
                            "B" "paper"
                            "C" "scissors")]
        [*reg-extension* (hash "X" "rock" "Y" "paper" "Z" "scissors")]
        [*fixed-extension* (hash "X" "lose" "Y" "draw" "Z" "win")])
    (list
     ((curry hash-ref) (hash-union *translator-base* *reg-extension*))
     ((curry hash-ref) (hash-union *translator-base* *fixed-extension*)))))

(match-define (list translate translate-fixed) translate-gen)

(define (hand-score play)
  (hash-ref (hash "rock" 1 "paper" 2 "scissors" 3) play))

(define (fix choice opponent)
  (let ([action (translate-fixed choice)])
    (cond [(equal?  action "lose") (rules opponent)]
          [(equal? action "win") (rules-rev opponent)]
          [else opponent])))

(define (game-result game-round [fixed? #f])
  (let* ([opponent (translate (first game-round))]
         [choice-modifier (if fixed? (λ (c) (fix c opponent)) translate)]
         [choice (choice-modifier (second game-round))]
         [hand-score (hand-score choice)])
    (cond [(equal? opponent choice) (+ 3 hand-score)]
          [(equal? (rules choice) opponent) (+ 6 hand-score)]
          [else hand-score])))

(define (calculate-results ledger [fixed? #f])
  (let ([game-result-fn (if fixed? (λ (r) (game-result r fixed?)) game-result)])
      (~>> ledger
           (string-split _ "\n")
           (map string-split)
           (map game-result-fn)
           (foldl + 0))))

(calculate-results roshambo)     ;; pt 1
(calculate-results roshambo #t)  ;; pt 2
