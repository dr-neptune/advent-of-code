#lang racket
(require racket advent-of-code)

(define calibration-values (fetch-aoc-input (find-session) 2023 1))

;; part 1
(define first-last-num-concat
  (compose (λ (ls) ((compose string->number list->string)
                    (list (first ls) (last ls))))
           (curry filter char-numeric?)
           string->list))

(foldl + 0
       ((compose (curry map first-last-num-concat) string-split)
        calibration-values))

;; part 2
(define (replace-nums s)
  (for/fold ([str s])
            ([r '(("one" "o1e")
                  ("two" "t2o")
                  ("three" "t3e")
                  ("four" "f4r")
                  ("five" "f5e")
                  ("six" "s6x")
                  ("seven" "s7n")
                  ("eight" "e8t")
                  ("nine" "n9e"))])
    (string-replace str (car r) (cadr r))))

(foldl + 0
       ((compose
         (curry map (compose first-last-num-concat replace-nums))
         string-split)
        calibration-values))
