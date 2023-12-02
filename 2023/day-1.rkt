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


(define word-to-digit
  (lambda (word)
    (cond ((string=? word "zero") 0)
          ((string=? word "one") 1)
          ((string=? word "two") 2)
          ((string=? word "three") 3)
          ((string=? word "four") 4)
          ((string=? word "five") 5)
          ((string=? word "six") 6)
          ((string=? word "seven") 7)
          ((string=? word "eight") 8)
          ((string=? word "nine") 9))))


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

(foldl + 0
       ((compose
         (curry map (compose (λ (ls) ((compose string->number list->string) (list (first ls) (last ls))))
                             (curry filter char-numeric?)
                             string->list))
         string-split
         replace-digit-words)
        calibration-values))

(define (find-number str)
  (for/fold ([acc '()]
             #:result (hash-ref mappings (list->string (reverse acc)) #f))
            ([char (string->list str)]
             #:break (hash-ref mappings (list->string (reverse acc)) #f))
    (values (cons char acc))))

(find-number "nineasldejfk")
