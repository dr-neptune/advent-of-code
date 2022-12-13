#lang racket
(require racket advent-of-code)

(define expackets
  #<<"
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"
  )

(define packets (fetch-aoc-input (find-session) 2022 13))

;; idea
;; parse these into lists
;; heavy abuse nested equal and flatten
;; ???
;; profit

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

;; comparator logic
(define (cmp a b)
  (cond [(andmap integer? (list a b))
         (cond [(< a b) #t]
               [(equal? a b) #f])]
        [(ormap integer? (list a b))
         (apply cmp (map (λ (v) (if (integer? v) (list v) v)) (list a b)))]
        [else (lscmp a b)]))


(cmp 1 2)
(cmp 2 1)
(cmp 2 '(1))
(cmp 1 '(2))

(define (lscmp a b)
  (begin
    (println (format "a: ~a b: ~a" a b))
    (cond
      [(andmap empty? (list a b)) #f]
      [(and (empty? a) (not (empty? b))) #t]
      [(and (empty? b) (not (empty? a))) #f]
      [(andmap list? (list a b))
       (or
        (lscmp (first a) (first b))
        (lscmp (rest a)(rest b)))]
      [else (cmp a b)])))


(lscmp lsa lsb)

(cmp '(1 1 3 1 1) '(1 1 5 1 1))              ;; should be #t
(cmp '((1)(2 3 4)) '((1) 4))                 ;; should be #t
(cmp '(9) '((8 7 6)))                        ;; should be #f
(cmp '((4 4) 4 4) '((4 4) 4 4 4))            ;; should be #t
(cmp '(7 7 7 7) '(7 7 7))                    ;; should be #f
(cmp '() '(3))                               ;; should be #t
(cmp '((())) '(()))                          ;; should be #f
(cmp '(1 (2 (3 (4 (5 6 7)))) 8 9)            ;; should be #f
     '(1 (2 (3 (4 (5 6 0)))) 8 9))


;; pt1
(foldl + 0 (map add1 (indexes-where (map (λ (p) (apply cmp p)) packet-pairs) (λ (v) (equal? v #t)))))
