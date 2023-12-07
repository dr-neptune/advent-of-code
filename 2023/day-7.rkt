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


#|

idea

pattern match on a sorted version of hand with a custom comparator (cmp 1)
use that to assign a score from strongest to weakest. Name this cmp 2

then use that matched comparator (cmp 2) to sort each item by rank

then group by output rank, and rank according to second set of rules (unmatched)

|#

(define hands
  (map (λ (card) (match-let ([(list hand score) (string-split card)])
                   (list (string->list hand) (string->number score))))
       camel-cards))

(define just-hands (map car hands))

;; card rank comparator
(define card-ranks
  (hash #\A 13 #\K 12 #\Q 11 #\J 10 #\T 9 #\9 8 #\8 7 #\7 6 #\6 5 #\5 4 #\4 3 #\3 2 #\2 1))

(define (compare-cards c1 c2)
  (> (hash-ref card-ranks c1) (hash-ref card-ranks c2)))


(define card-cmp (comparator-of-constants #\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))

;; (define equiv-hand-cmp
;;   (make-comparator
;;    (λ (a b)
;;      (cond [()]))))

(define hand-cmp
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




(transduce '(#\Q #\A #\8)
           (sorting card-cmp)
           #:into into-list)


(transduce just-hands
           (sorting hand-cmp)
           #:into into-list)


(define card-comp
  (make-comparator
   (λ (a b)
     (for/last ([a-val a]
           [b-val b])
       (displayln (format "~a ~a" a-val b-val))
       (let ([cmp (compare card-cmp a-val b-val)])
         (match cmp
           [equivalent 'pass]
           [lesser lesser]
           [greater greater]))))))



(transduce just-hands
           (sorting card-comp)
           #:into into-list)


(transduce just-hands
           (sorting (comparator-map hand-cmp) (comparator-map card-cmp just-hands))
           #:into into-list)




;; (map
;;  (λ (hand)
;;    (transduce hand (sorting card-cmp) #:into into-list))
;;  (transduce just-hands
;;            (sorting hand-cmp)
;;            #:into into-list))


;; maybe use comparator-chain to handle the several levels

(for/list ([hand just-hands])
  (cons
   hand
   (match (sort (map length (group-by identity hand)) <)
    ['(5) 1] ;; 5 of a kind
    ['(1 4) 2] ;; 4 of a kind
    ['(2 3) 3] ;; full house
    ['(1 1 3) 4] ;; 3 of a kind
    ['(1 2 2) 5] ;; 2 pair
    ['(1 1 1 2) 6]  ;; one pair
    ['(1 1 1 1 1) 7] ;; distinct
    [_ 8]))) ;; nothing


(((#\3 #\2 #\T #\3 #\K) . 6)  ;; 1 pair
 ((#\T #\5 #\5 #\J #\5) . 4)  ;; 3 of a kind
 ((#\K #\K #\6 #\7 #\7) . 5)  ;; 2 pair
 ((#\K #\T #\J #\J #\T) . 5)  ;; 2 pair
 ((#\Q #\Q #\Q #\J #\A) . 4)) ;; 3 of a kind

'((#\J #\T #\5 #\5 #\5)
  (#\A #\Q #\Q #\Q #\J)
  (#\K #\K #\7 #\7 #\6)
  (#\K #\J #\J #\T #\T)
  (#\K #\T #\3 #\3 #\2))

'((#\T #\5 #\5 #\J #\5)
  (#\Q #\Q #\Q #\J #\A)
  (#\K #\K #\6 #\7 #\7)
  (#\K #\T #\J #\J #\T)
  (#\3 #\2 #\T #\3 #\K))
