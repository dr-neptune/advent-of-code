(require racket threading advent-of-code)


(define pws (~> (fetch-aoc-input (find-session) 2020 2)
                (string-split "\n")
                ((curry map (curryr string-split ": ")))))

(define (try func)
  (λ (x)
    (let ([result (func x)])
      (if result result x))))

;; part 1
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

(for/sum ([pw pws]
          #:when (match-let ([(list rule pw) pw])
                   (valid-password? rule pw)))
  1)

;; part 2
(define (valid-password? rule pw)
  (match-let* ([(list _ min-index max-index) (map (try string->number)
                                                  (regexp-match #px"^([0-9]+)-([0-9]+)" rule))]
               [(list _ ... rule-char) (string->list rule)])
    (let ([check-point
           (λ (rule-idx)
             (equal? rule-char (list-ref (string->list pw) (sub1 rule-idx))))])
      (xor (check-point min-index) (check-point max-index)))))


(for/sum ([pw pws]
          #:when (match-let ([(list rule pw) pw])
                   (valid-password? rule pw)))
  1)
