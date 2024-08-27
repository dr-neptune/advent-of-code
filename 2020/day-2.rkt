(require racket threading advent-of-code)


(define pws (~> (fetch-aoc-input (find-session) 2020 2)
                (string-split "\n")
                ((curry map (curryr string-split ": ")))))

(define pws '(("1-3 a" "abcde")
              ("1-3 b" "cdefg")
              ("2-9 c" "ccccccccc")))

(define (valid-password? rule pw)
  (let* ([rule-chars (string->list rule)]
         [pw-chars (string->list pw)]
         [chosen-letter (last rule-chars)]
         [min-max-counts (regexp-match #px"^([0-9]+)-([0-9]+)" rule)]
         [min-count (string->number (list-ref min-max-counts 1))]
         [max-count (string->number (list-ref min-max-counts 2))]
         [num-letters-in-pw (count (curry eq? chosen-letter) pw-chars)])
    (and (<= min-count num-letters-in-pw)
         (>= max-count num-letters-in-pw))))


;; part 1
(for/sum ([pw pws]
           #:when (match-let ([(list rule pw) pw])
                    (valid-password? rule pw)))
  1)

;; part 2
