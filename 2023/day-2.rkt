#lang racket
(require racket advent-of-code)

(define bag-draws (fetch-aoc-input (find-session) 2023 2)

  )

(string-split ex-bag-draws "\n")

(define ex-bag-draws "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(define inter (map (λ (s) (map string-trim (string-split s ";"))) (string-split ex-bag-draws "\n")))



'(("Game 1: 3 blue, 4 red" "1 red, 2 green, 6 blue" "2 green")
  ("Game 2: 1 blue, 2 green" "3 green, 4 blue, 1 red" "1 green, 1 blue")
  ("Game 3: 8 green, 6 blue, 20 red" "5 blue, 4 red, 13 green" "5 green, 1 red")
  ("Game 4: 1 green, 3 red, 6 blue" "3 green, 6 red" "3 green, 15 blue, 14 red")
  ("Game 5: 6 red, 1 blue, 3 green" "2 blue, 1 red, 2 green"))

(string-split (first (string-split ex-bag-draws "\n")) ";")
 (regexp-replace* #px"Game \\d+: " (first (string-split (first (string-split ex-bag-draws "\n")) ";")) "")

(define (get-draws games)
  (map ))

#|

idea

parse each line, keep a max for each draw

|#

;; get vals for regular hand
(map
 (λ (s) (let ([vals (string-split s)])
          (cons (string->number (first vals))
                (last vals))))
 (map string-trim (string-split "3 green, 4 blue, 1 red" ",")))


;; get game and number, then remove from string
(regexp-replace* #px"Game \\d+: " "Game 1: 3 blue, 4 red" "")

(first (filter char-numeric? (string->list (first (regexp-match* #px"Game \\d+:" "Game 1: 3 blue, 4 red")))))
