#lang racket
(require racket racket/hash threading advent-of-code)

(define sues
  (~> (fetch-aoc-input (find-session) 2015 16)
      (string-split "\n")))

(define sue-hsh
  (for/hash ([sue sues] [idx (in-naturals 1)])
    (values idx
          (match-let ([(list* _ attributes) (string-split sue ", ")])
            (~> (map (λ (item)
                       (~> item
                           (string-split ": ")
                           ((λ (ls) (cons (first ls) (string->number (cadr ls)))))))
                     attributes)
                make-immutable-hash)))))

(define mfc-hsh
  (make-immutable-hash
   '(["children" . 3]
     ["cats" . 7]
     ["samoyeds" . 2]
     ["pomeranians" . 3]
     ["akitas" . 0]
     ["vizslas" . 0]
     ["goldfish" . 5]
     ["trees" . 3]
     ["cars" . 2]
     ["perfumes" . 1])))

(for/list ([(sue-num sue-attr) (in-hash sue-hsh)]
           #:do [(define both (hash-intersect
                               sue-attr mfc-hsh
                               #:combine/key
                               (λ (k v1 v2) (if (not (equal? v1 v2)) -1 v1))))]
           #:when (false? (member -1 (hash-values both))))
  sue-num)

;; guessed since there were only 4 possibilities. The correct one was 103

;; part 2
;; my weak answer to pt 1 may be a problem here
(for/list ([(sue-num sue-attr) (in-hash sue-hsh)]
           #:do [(define both (hash-intersect
                               sue-attr mfc-hsh
                               #:combine/key
                               (λ (k v1 v2)
                                 (match k
                                   [(or "cats" "trees")
                                    (if (not (> v1 v2)) -1 v1)]
                                   [(or "pomeranians" "goldfish")
                                    (if (not (< v1 v2)) -1 v1)]
                                   [_ (if (not (equal? v1 v2)) -1 v1)]))))]
           #:when (false? (member -1 (hash-values both))))
  sue-num)

;; oi vey, this is not solved
