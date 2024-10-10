#lang racket
(require racket threading advent-of-code)

(define foods
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(define foods (fetch-aoc-input (find-session) 2020 21))

(define parsed-food
  (~>> foods
       (string-split _ "\n")
       (map (λ (line)
              (match-let* ([(list ingredients allergens) (string-split line " (contains ")]
                           [ingredients (string-split ingredients)]
                           [allergens (~> allergens
                                          (string-replace ")" "")
                                          (string-split ", ")
                                          (map string-trim _))])
                (list ingredients allergens))))))

;; part 1
(define shared-allergens
  (let ([hsh (make-hash)])
    (for* ([food parsed-food]
           #:do [(match-define (list food-ls allergens) food)]
           [allergen allergens])
      (hash-update! hsh allergen (curry set-intersect (list->set food-ls)) (list->set food-ls)))
    hsh))

(let* ([allergenic (apply set-union (hash-values shared-allergens))]
       [all-food (apply set-union (map (λ~> first list->set) parsed-food))]
       [safe-food (set-subtract all-food allergenic)]
       [foods-ls (flatten (map first parsed-food))])
  (for/sum ([safe safe-food])
    (count (curry equal? safe) foods-ls)))

;; part 2
(let ([pairs '()])
  (let loop ([hsh (hash-copy shared-allergens)])
    (for ([(allergen ingredients) (in-hash hsh)]
          #:when (equal? 1 (set-count ingredients)))
      (begin
        (set! pairs (cons (cons allergen (set-first ingredients)) pairs))
        (loop (hash-map/copy hsh (λ (k v) (values k (remove (set-first ingredients) (set->list v)))))))))
  (string-join (map cdr (sort (remove-duplicates pairs) #:key car string<?)) ","))
