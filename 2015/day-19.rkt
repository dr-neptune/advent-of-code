#lang racket
(require racket advent-of-code threading)

(define (flatten-depth lst [depth 1])
  (cond [(<= depth 0) lst]
        [(not (list? lst)) (list lst)]
        [else (append-map (lambda (x) (flatten-depth x (sub1 depth))) lst)]))

(match-define (list rxns molecule)
  (~> (fetch-aoc-input (find-session) 2015 19)
  (string-split "\n\n")))

(define rxns
  (~> rxns
      (string-split "\n")
      (map (curryr string-split " => ") _)))

(define rxp #rx"[A-Z][a-z]?")

(define molecule
  (~> molecule
      (regexp-match* rxp _)))


;; part 1
(~>> molecule length range
     (map (λ (idx)
            (let* ([possible-rxns (filter (λ (rxn) (equal? (list-ref molecule idx) (first rxn))) rxns)]
                   [rxn-products (map second possible-rxns)])
              (map (λ (v) (list-set molecule idx v)) rxn-products))))
     flatten-depth
     (map (curryr string-join ""))
     remove-duplicates
     length)


(define (get-possibles molecule)
  (~>> molecule length range
     (map (λ (idx)
            (let* ([possible-rxns (filter (λ (rxn) (equal? (list-ref molecule idx) (first rxn))) rxns)]
                   [rxn-products (map second possible-rxns)])
              (map (λ (v) (list-set molecule idx v)) rxn-products))))
     flatten-depth
     (map (curryr string-join ""))
     remove-duplicates))

(~>> molecule length range
     (map (λ (idx)
            (let* ([possible-rxns (filter (λ (rxn) (equal? (list-ref molecule idx) (first rxn))) rxns)]
                   [rxn-products (map second possible-rxns)])
              (map (λ (v) (list-set molecule idx v)) rxn-products))))
     flatten-depth
     (map (curryr string-join ""))
     remove-duplicates
     length)

;; part 2
#|

we must start with electrons and make each molecule

breadth-first-search starting from a single e, then we recursively replace molecule bits until we
have our result. Maybe now we have a use for cartesian-product

growth is essentially unbounded here
so we need to do something like a for/first when we hit the molecule
and we need to count the number of steps to get there

|#

(define rxns
  (~> "e => H
e => O
H => HO
H => OH
O => HH"
      (string-split "\n")
      (map (curryr string-split " => ") _)))


(filter (λ~> first (equal? "e")) rxns)

(define (react val)
  (let ([possibles (filter (λ~> first (equal? val)) rxns)])
    (map second possibles)))


;; if in rxns, use it
;; if not, decompose and see what can be done
;; rewrite the solution from part 1 to take in a string and return the possible outputs
(define (react val)
  (let ([possibles (filter (λ~> first (equal? val)) rxns)])
    (map second possibles)))


(flatten (map react (react "e")))

(for/fold ([candidates (react "e")])
          ([idx (in-range 3)])
  (displayln candidates)
  (flatten (map react candidates)))
