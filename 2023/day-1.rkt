#lang racket
(require racket advent-of-code)

(define calibration-values (fetch-aoc-input (find-session) 2023 1))

;; part 1
(foldl + 0
       ((compose
         (curry map (compose (λ (ls) ((compose string->number list->string)
                                      (list (first ls) (last ls))))
                             (curry filter char-numeric?)
                             string->list))
         string-split)
        calibration-values))

;; part 2
(foldl + 0
       ((compose
         (curry map (compose (λ (ls) ((compose string->number list->string) (list (first ls) (last ls))))
                             (curry filter char-numeric?)
                             string->list))
         string-split)
        calibration-values))

(define mappings
  (hash "zero" 0
        "one" 1
        "two" 2
        "three" 3
        "four" 4
        "five" 5
        "six" 6
        "seven" 7
        "eight" 8
        "nine" 9))

(define exstr "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(define (digit->char n) (integer->char (+ n 48)))
(define (string-reverse str) ((compose list->string reverse string->list) str))

;; (second (string-split exstr))

;; (let loop ([str-ls (string->list (second (string-split exstr)))]
;;            [str-acc '()])
;;   (let ([condensed-acc (hash-ref mappings (list->string (reverse str-acc)) #f)])
;;     (cond [(empty? str-ls) (list->string (reverse str-acc))]
;;           [condensed-acc (list->string (cons (digit->char condensed-acc) str-ls))]
;;           [else (loop (rest str-ls) (cons (first str-ls) str-acc))])))

;; (let loop ([str-ls (reverse '(#\8 #\w #\o #\t #\h #\r #\e #\e))]
;;            [str-acc '()])
;;   (let ([condensed-acc (hash-ref mappings (list->string str-acc) #f)])
;;     (cond [(empty? str-ls) (list->string (reverse str-acc))]
;;           [condensed-acc (list->string (cons (digit->char condensed-acc) str-ls))]
;;           [else (loop (rest str-ls) (cons (first str-ls) str-acc))])))



(define (replace-leading-text-num str [direction reverse])
  (let loop ([str-ls (string->list str)]
             [str-acc '()])
    (displayln str-acc)
    (let ([condensed-acc (hash-ref mappings (list->string (direction str-acc)) #f)])
      (cond [(empty? str-ls) (list->string (reverse str-acc))]
            [condensed-acc (list->string (cons (digit->char condensed-acc) str-ls))]
            [else (loop (rest str-ls) (cons (first str-ls) str-acc))]))))

(replace-leading-text-num (reverse (replace-leading-text-num exstr)))

(replace-leading-text-num (second (string-split exstr)))
(replace-leading-text-num (string-reverse (replace-leading-text-num (second (string-split exstr)))))
(replace-leading-text-num (second (string-split exstr)))

(define (replace-first-and-last-word-digits str)
  (string-reverse
   (replace-leading-text-num
    (string-reverse (replace-leading-text-num str))
    identity)))

(replace-first-and-last-word-digits (third (string-split exstr)))

;; (replace-leading-text-num
;;  (string-reverse (replace-leading-text-num (second (string-split exstr))))
;;  identity)

(map replace-first-and-last-word-digits (string-split exstr))

#|

idea

We have a list of character digits
and we want to find the first instance of a word that is a number

ex: '(a b c o n e 2 t h r e e x y z)

->  '(a b c 1 2 three x y z)



|#

(indexes-of '(#\a #\b #\c #\o #\n #\e #\2 #\t #\h #\r #\e #\e #\x #\y #\z))

(string-contains? (third (string-split exstr)) "one")
