#lang racket
(require racket threading advent-of-code)

(define (parse-muls str)
  (define rxpat #rx"mul\\([0-9]+,[0-9]+\\)")
  (~>> str
      (regexp-match* rxpat _)
      (map (λ (s) (~> s
                      (substring 4 (sub1 (string-length s)))
                      (string-split ",")
                      (map string->number _))))))

(define memory (fetch-aoc-input (find-session) 2024 3 #:cache #t))

(define (dprod ls) (for/sum ([ele ls]) (apply * ele)))

;; part 1
(~> memory parse-muls dprod)

;; part 2
(define dont_do_pat #rx"don't\\(\\).*?do\\(\\)")

(let* ([drop-matches (regexp-match* dont_do_pat memory)])
  (for/fold ([mem memory]
             #:result (~> mem parse-muls dprod))
            ([mat drop-matches])
    (string-replace mem mat "")))
