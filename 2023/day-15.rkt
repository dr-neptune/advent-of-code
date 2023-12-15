#lang racket
(require racket advent-of-code)

(define init-seq (map (curryr string-replace "\n" "") (string-split (fetch-aoc-input (find-session) 2023 15) ",")))

(define init-seq (string-split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ","))

(define (Holiday-ASCII-String-Helper-algorithm_appendix_1A str)
  (let ([current-value 0])
    (let loop ([ascii-values (map char->integer (string->list str))])
      (if (empty? ascii-values)
          current-value
          (begin
            (set! current-value (remainder (* 17 (+ current-value (first ascii-values))) 256))
            (loop (rest ascii-values)))))))


;; part 1
(apply + (map Holiday-ASCII-String-Helper-algorithm_appendix_1A init-seq))

;; part 2
