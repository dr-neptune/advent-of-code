#lang racket
(require racket threading advent-of-code)

#|

idea

things missing from the list aren't 0, you just don't remember
so missing items are a ?

mfc

sues

for each sue in sues
see how many items are in common with mfc

|#

(define sues
  (~> (fetch-aoc-input (find-session) 2015 16)
      (string-split "\n")))

(define sue-hsh
  (make-hash
   (for/list ([sue sues] [idx (in-naturals 1)])
     (cons idx
           (match-let ([(list* _ attributes) (string-split sue ", ")])
             (~> (map (λ (item)
                        (~> item
                            (string-split ": ")
                            ((λ (ls) (cons (first ls) (string->number (cadr ls)))))))
                      attributes)
                 make-hash))))))

(define mfc-hsh
(make-hash (["children" . 3]
["cats" . 7]
["samoyeds" . 2]
["pomeranians" . 3]
["akitas" . 0]
["vizslas" . 0]
["goldfish" . 5]
["trees" . 3]
["cars" . 2]
["perfumes" . 1]))
)

(for/list ([(sue-num sue-attr) (in-hash sue-hsh)])
  )
