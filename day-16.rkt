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
"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"
   (string-split _ "\n\n")
   (map (λ~> (string-split "\n")))
   ((λ (tri)
      (let ([parse-tickets (curry map parse-ticket)])
        (list (parse-fields (first tri))
              (parse-tickets (drop (second tri) 1))
              (parse-tickets (drop (third tri) 1))))))))

(pretty-display ticket-info)

#|

we need to check for tickets that are not valid for any field
maybe we can take each range set and iterate over it for each value?

|#

(pretty-display
 (let ([ranges (first ticket-info)] [tickets (third ticket-info)])
  (for*/list ([ticket tickets])
    (for/list ([field-ranges (map field-ranges (first ticket-info))])
      (for/list ([field-range field-ranges])
      (map (λ~>> (range-contains? field-range)) ticket))))))

#|

now we have a data structure

|#
;;    1-3        5-7          6-11       33-44       13-40       45-50
((((#f #t #f) (#t #f #f)) ((#t #f #f) (#f #f #f)) ((#f #f #f) (#f #f #t)))
 (((#f #f #f) (#f #f #f)) ((#f #f #f) (#t #f #f)) ((#t #f #f) (#f #f #t)))
 (((#f #t #f) (#f #f #f)) ((#f #f #f) (#f #f #f)) ((#f #f #t) (#f #f #f)))
 (((#f #f #f) (#f #t #f)) ((#f #t #f) (#t #f #f)) ((#t #f #f) (#f #f #f))))
;; might be a problem here where a number fits multiple fields. In this case
;; the rules are disjoint, but they may not be in the full output

(pretty-display
 (let ([ranges (first ticket-info)] [tickets (third ticket-info)])
   (for*/list ([ticket tickets])
     (displayln ticket)
     (for/list ([field-ranges (map field-ranges (first ticket-info))])
       ((compose (curry ormap identity) flatten)
        (for/list ([field-range field-ranges])
          (map (λ~>> (range-contains? field-range)) ticket)))))))

;; hmm, our ts and fs aren't lining up
;; we should simplify it and just iterate over all the ranges since they are disjoint
;; for each number. Then, if a number is all #f, we add it to a list
