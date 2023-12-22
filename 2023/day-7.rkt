#lang racket
(require racket
         advent-of-code
         rebellion/base/comparator
         rebellion/streaming/transducer
         rebellion/collection/list)

(define camel-cards (string-split (fetch-aoc-input (find-session) 2023 7) "\n"))

(define camel-cards (string-split "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483" "\n"))


(define hands
  (for/hash ([hand-bid (map (λ (card) (match-let ([(list hand score) (string-split card)])
                                        (list (string->list hand) (string->number score))))
                            camel-cards)])
    (values (first hand-bid) (second hand-bid))))

(define card-order-rank<=>
  (let ([card-cmp
         (comparator-of-constants #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A)])
    (make-comparator
     (λ (hand1 hand2)
       (let ([cmp (λ (a b) (compare card-cmp a b))])
         (for/first ([c1  hand1] [c2 hand2]
                                 #:when (not (equal? equivalent (cmp c1 c2))))
           (match (cmp c1 c2)
             [lesser lesser]
             [greater greater])))))))


;; (compare card-order-rank<=> '(#\T #\5 #\5 #\J #\5) '(#\Q #\Q #\Q #\J #\A))


(define hand-rank<=>
  (make-comparator
   (λ (a b)
     (let ([hand-mapping
            (λ (hand) (match (sort (map length (group-by identity hand)) <)
                        ['(5) 1] ;; 5 of a kind
                        ['(1 4) 2] ;; 4 of a kind
                        ['(2 3) 3] ;; full house
                        ['(1 1 3) 4] ;; 3 of a kind
                        ['(1 2 2) 5] ;; 2 pair
                        ['(1 1 1 2) 6]  ;; one pair
                        ['(1 1 1 1 1) 7] ;; distinct
                        [_ 8]))])
       (cond [(< (hand-mapping a) (hand-mapping b)) lesser]
             [(= (hand-mapping a) (hand-mapping b)) equivalent]
             [(> (hand-mapping a) (hand-mapping b)) greater])))))


;; chained sort is working!
(define sorted-hands
  (transduce (hash-keys hands)
             (sorting (comparator-chain hand-rank<=> (comparator-reverse card-order-rank<=>)))
             #:into into-list))

(apply + (map (λ (a b) (* (hash-ref hands a) b))
              sorted-hands (stream->list (in-inclusive-range (length sorted-hands) 1 -1))))

;; part 2
(define hand-rank<=>
  (make-comparator
   (λ (a b)
     (let ([hand-mapping
            (λ (hand) (match (sort (map length (group-by identity hand)) <)
                        ['(5) 1] ;; 5 of a kind
                        ['(1 4) 2] ;; 4 of a kind
                        ['(2 3) 3] ;; full house
                        ['(1 1 3) 4] ;; 3 of a kind
                        ['(1 2 2) 5] ;; 2 pair
                        ['(1 1 1 2) 6]  ;; one pair
                        ['(1 1 1 1 1) 7] ;; distinct
                        [_ 8]))])
       (cond [(< (hand-mapping a) (hand-mapping b)) lesser]
             [(= (hand-mapping a) (hand-mapping b)) equivalent]
             [(> (hand-mapping a) (hand-mapping b)) greater])))))

(compare hand-rank<=> '(#\T #\5 #\5 #\J #\5) '(#\Q #\Q #\Q #\T #\A))

(group-by (λ (v) identity)
          '(#\T #\5 #\5 #\J #\5))

;; idea
;; replace J with max value
(let ([max-val]))
