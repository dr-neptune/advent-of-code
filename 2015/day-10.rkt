#lang racket

(require racket)

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (group->number group)
  ((λ (ls) (+ (* 10 (length ls)) (char->digit (first ls)))) group))

(define (look-and-say str-digit)
  (for/fold ([items '()]
             [all-items '()]
             #:result (string-join (map (compose number->string group->number)
                                        (reverse (cons items all-items))) ""))
            ([digit (string->list str-digit)])
    (if (empty? items)
        (values (list digit) all-items)
        (let ([prev-item (first items)])
          (if (equal? prev-item digit)
              (values (cons digit items) all-items)
              (values (list digit) (cons items all-items)))))))


;; part 1 is 40, part 2 is 50
(for/fold ([res "1113122113"]
           #:result (string-length res))
          ([_ (in-range 50)])
  (look-and-say res))
