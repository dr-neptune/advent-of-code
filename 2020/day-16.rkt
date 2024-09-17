#lang racket
(require racket threading advent-of-code
         rebellion/base/range)

(define-struct field (name ranges))

(define (parse-ticket ticket)
  (map string->number (string-split ticket ",")))

(define (parse-field field-str)
  "Parses a field string into a `field` struct, handling potential errors."
  (define split-result (string-split field-str ": "))
  (if (= (length split-result) 2)
      (let ([name (first split-result)]
            [ranges-part (second split-result)])
        (define range-strings (string-split ranges-part " or "))
        (define ranges (map parse-range range-strings))
        (field name ranges))
      (error 'parse-field "Expected two parts (name and ranges) in the field string, got: ~a" field-str)))

(define (parse-range range-str)
  "Parses a range string like '1-3' into a Range object."
  (define split-result (map string->number (string-split range-str "-")))
  (define start (first split-result))
  (define end (second split-result))
  (closed-range start end))


(define ticket-info
  (~>>
;; "class: 1-3 or 5-7
;; row: 6-11 or 33-44
;; seat: 13-40 or 45-50

;; your ticket:
;; 7,1,14

;; nearby tickets:
;; 7,3,47
;; 40,4,50
;; 55,2,20
;; 38,6,12"
"class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"
;;   (fetch-aoc-input (find-session) 2020 16)
   (string-split _ "\n\n")
   (map (λ~> (string-split "\n")))
   ((λ (tri)
      (let ([parse-tickets (curry map parse-ticket)]
            [parse-fields (curry map parse-field)])
        (list (parse-fields (first tri))
              (parse-tickets (drop (second tri) 1))
              (parse-tickets (drop (third tri) 1))))))))


;; part 1
(let* ([ranges (first ticket-info)]
       [tickets (third ticket-info)]
       [field-ranges (flatten (map field-ranges (first ticket-info)))]
       [valid-number? (λ (num)
                        (ormap identity (map (λ (range) (range-contains? range num)) field-ranges)))])
  (for*/sum ([ticket tickets]
             [ticket-val ticket]
             #:when (false? (valid-number? ticket-val)))
    ticket-val))

;; part 2
#|

idea

even though we just need departure, we still need to find out all the field matches

let's start by discarding the invalid numbers

|#
(define invalid-numbers
  (let* ([ranges (first ticket-info)]
         [tickets (third ticket-info)]
         [field-ranges (flatten (map field-ranges (first ticket-info)))]
         [valid-number? (λ (num)
                          (ormap identity (map (λ (range) (range-contains? range num)) field-ranges)))])
    (for*/list ([ticket tickets]
                [ticket-val ticket]
                #:when (false? (valid-number? ticket-val)))
      ticket-val)))

;; if a ticket contains any invalid value, drop it entirely
(define (list-contains-any? base-list membership-list)
  (ormap (lambda (x) (not (not (member x membership-list)))) base-list))

(define valid-tickets
  (filter
   (compose not (curryr list-contains-any? invalid-numbers))
   (third ticket-info)))


#|

idea
which permutation of the first value fits the ranges?
of the second?

can we skimp out and only do a couple?

|#

(let* ([field-ranges (flatten (map field-ranges (first ticket-info)))]
       [check-match
        (λ (permutation)
          (andmap identity (map (λ (range val) (range-contains? range val)) field-ranges permutation)))])
  (for/list ([ticket valid-tickets])
    (for/list ([perm (in-permutations ticket)]
               ;; #:when (check-match perm)
               )
      (list
       perm
       (map (λ (range val) (range-contains? range val)) field-ranges perm))
      )))

;; need to get a position
;; maybe we should permute the ranges instead?
;; maybe it wouldn't matter, think about it though
