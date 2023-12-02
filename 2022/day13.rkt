#lang racket
(require racket threading advent-of-code)

(define packets (fetch-aoc-input (find-session) 2022 13))

;; given input instructions, output a list of lists with proper nesting
(define (packet->racket packets)
  (let ([make-it-rack
         (foldl (λ (changes str)
                  (string-replace str (first changes) (second changes)))
                packets
                '(("[" "(")
                  ("]" ")")
                  ("," " ")))])
    (map (λ (pair) (map (λ (ls) (with-input-from-string ls read))
                        (string-split pair "\n")))
         (string-split make-it-rack "\n\n"))))

(define packet-pairs (packet->racket packets))

;; comparator
(define (cmp a b)
  (define (get-results a b)
    (cond [(and (integer? a) (integer? b))
                  (cond [(< a b) #t]
                        [(< b a) #f]
                        [else 'pass])]
          ;; handle int / list
          [(ormap integer? (list a b))
           (apply cmp (map (λ (v) (if (integer? v) (list v) v)) (list a b)))]
          ;; list / list
          [else
           (cond [(and (empty? a) (not (empty? b))) #t]
                 [(and (empty? b) (not (empty? a))) #f]
                 [(andmap empty? (list a b)) 'pass]
                 [else
                  (cons
                   (get-results (first a) (first b))
                   (get-results (rest a) (rest b)))])]))
    (filter boolean? (flatten (get-results a b))))

;; pt1
(~>> (map (λ (p) (first (apply cmp p))) packet-pairs)
     (indexes-where _ (λ (v) (equal? v #t)))
     (map add1)
     (foldl + 0))

;; pt2
;; flatten the input list
(define all-packets
  (let results ([pairs packet-pairs]
                [new-ls '(((2)) ((6)))])                  ;; add divider packets
    (cond [(empty? pairs) new-ls]
          [else (results (rest pairs)
                         (append new-ls (list (caar pairs) (cadar pairs))))])))

;; now we need to use cmp as a comparator to sort them all
(define sorted-packets (sort all-packets (λ (a b) (first (cmp a b)))))

(~>> sorted-packets
     (indexes-where _ (λ (v) (member v '(((2)) ((6))))))
     (map add1)
     (apply *))
