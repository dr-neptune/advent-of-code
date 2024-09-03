#lang racket
(require racket threading advent-of-code)

(define seats (~> (fetch-aoc-input (find-session) 2020 5) (string-split "\n")))

(define (check seats lval rval fchar schar)
  (for/fold ([left lval] [right rval]
                      #:result left)
            ([seat (string->list seats)])
    (let ([mid-point (quotient (+ left right) 2)])
    (match seat
      [(== fchar) (values left mid-point)]
      [(== schar) (values (add1 mid-point) right)]
      [_ (values mid-point mid-point)]))))

(define (check-pass seats)
  (match-define (list rows cols)
    ((compose flatten list)
     (regexp-replace #rx"[LR].*" seats "")
     (regexp-match #rx"[LR].*" seats)))
  (list (check rows 0 127 #\F #\B)
        (check cols 0 7 #\L #\R)))

(map (compose (curry apply (λ (a b) (+ (* a 8) b))) check-pass)
     seats)

;; part 1
(apply max seat-ids)

;; part 2
(for/first ([seat (sort seat-ids <)]
            [idx (in-naturals (apply min seat-ids))]
            #:when (not (equal? idx seat)))
  idx)
