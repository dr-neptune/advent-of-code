#lang racket
(require racket threading advent-of-code)

(define cups
  (~>> "389125467"
       (string-split _ "")
       (filter (compose not (curry equal? "")))
       (map string->number)))


;; let's try simulating with in-cycle
(define (stream-take-until st v)
  (if (equal? (stream-first st) v)
      st
      (stream-take-until (stream-rest st) v)))

(define (list-reorder-first ls v)
  (let ([val-idx (index-of ls v)])
    (append (drop ls val-idx)
            (take ls val-idx))))

(~> (for/fold ([curr-idx 0] [cups cups] #:result cups)
              ([idx (in-range 0 10)])
  (let* ([current-cup-idx curr-idx]
         [current-cup (list-ref cups current-cup-idx)]
         [pick-up (take (drop cups (add1 current-cup-idx)) 3)]
         [unordered-subset-circle (filter (λ (v) (not (member v pick-up))) cups)]
         [ordered-subset-circle (sort unordered-subset-circle <)]
         [destination (let ([curr-cup-sorted-idx (index-of ordered-subset-circle current-cup)])
                        (if (zero? curr-cup-sorted-idx)
                            (last ordered-subset-circle)
                            (list-ref ordered-subset-circle (sub1 curr-cup-sorted-idx))))]
         [new-order (list-reorder-first (list-insert unordered-subset-circle
                                                     (add1 (index-of unordered-subset-circle destination)) pick-up)
                                        current-cup)])
    (values 1 new-order)))
    (list-reorder-first 1)
    (drop 1))

#|

part 2
idea

likely need to use constant time data structures

step 0: make a stream implementation!
step 1: refactor old version
step 2: convert to use vector

|#

;; (define (list-reorder-first ls v)
;;   (let ([val-idx (index-of ls v)])
;;     (append (drop ls val-idx)
;;             (take ls val-idx))))

;; (~> (for/fold ([curr-idx 0] [cups cups] #:result cups)
;;               ([idx (in-range 0 10)])
;;       (let* ([current-cup-idx curr-idx]
;;              [current-cup (list-ref cups current-cup-idx)]
;;              [pick-up (take (drop cups (add1 current-cup-idx)) 3)]
;;              [unordered-subset-circle (filter (λ (v) (not (member v pick-up))) cups)]
;;              [ordered-subset-circle (sort unordered-subset-circle <)]
;;              [destination (let ([curr-cup-sorted-idx (index-of ordered-subset-circle current-cup)])
;;                             (if (zero? curr-cup-sorted-idx)
;;                                 (last ordered-subset-circle)
;;                                 (list-ref ordered-subset-circle (sub1 curr-cup-sorted-idx))))]
;;              [new-order (list-reorder-first (list-insert unordered-subset-circle
;;                                                          (add1 (index-of unordered-subset-circle destination)) pick-up)
;;                                             current-cup)])
;;         (values 1 new-order))))

#|

notes:

we know that the numbers after the highest in the initial list are > max initial
so we should be able to optimize around the destination selection

also, we probably don't need to do a stream filter to not take a pick-up value.
Since there are only 3 pick-up values, we can just filter those

steps:
1. get current cup label
2. take next 3 cups
3. select destination cup
4. place new cups
5. move to new current cup

|#

(define (list->stream-cycle ls)
  ((λ~> in-list in-cycle sequence->stream) ls))

(define (get-destination curr-label picked-up sorted-initial-input)
  (for/first ([in (~> sorted-initial-input
                      list->stream-cycle
                      (stream-take-until curr-label)
                      stream-rest)]
              #:when (false? (member in picked-up)))
    in))

(get-destination 3 '(8 9 1) (sort '(3 8 9 1 2 5 4 6 7) >))
(get-destination 2 '(8 9 1) (sort '(3 8 9 1 2 5 4 6 7) >))
(get-destination 5 '(4 6 7) (sort '(3 8 9 1 2 5 4 6 7) >))
(get-destination 8 '(9 1 3) (sort '(3 8 9 1 2 5 4 6 7) >))
(get-destination 4 '(6 7 9) (sort '(3 8 9 1 2 5 4 6 7) >))
(get-destination 1 '(3 6 7) (sort '(3 8 9 1 2 5 4 6 7) >))


#|

to boost to 1m rows, just append 1m to the front of the sorted stream?
might req more thought

|#
(let ([input-cups cups] [in-cups-sorted (sort cups >)])
  (let loop ([cups (list->stream-cycle input-cups)]
             [idx 0])
    (let* ([curr-cup-label (stream-first cups)]
           [pick-up (stream-take (stream-rest cups) 3)]
           [destination (get-destination curr-cup-label (stream->list pick-up) in-cups-sorted)])
      (begin (displayln (format "curr: ~a pu: ~a dest: ~a idx: ~a" curr-cup-label (stream->list pick-up) destination idx))
             (if (equal? idx 10)
                 (stream->list cups)
                 (loop (stream-take-until
                        (stream-append pick-up (stream-rest (stream-take-until cups destination)))
                        destination)
                       (add1 idx))))
      )))

#|

incorrect!

curr: 3 pu: (8 9 1) dest: 2 idx: 0
curr: 2 pu: (5 4 6) dest: 1 idx: 1
curr: 1 pu: (2 5 4) dest: 9 idx: 2
curr: 9 pu: (1 2 5) dest: 8 idx: 3
curr: 8 pu: (9 1 2) dest: 7 idx: 4
curr: 7 pu: (3 8 9) dest: 6 idx: 5
curr: 6 pu: (7 3 8) dest: 5 idx: 6
curr: 5 pu: (4 6 7) dest: 3 idx: 7
curr: 3 pu: (8 9 1) dest: 2 idx: 8
curr: 2 pu: (5 4 6) dest: 1 idx: 9
curr: 1 pu: (2 5 4) dest: 9 idx: 10


|#

(println "hello")
