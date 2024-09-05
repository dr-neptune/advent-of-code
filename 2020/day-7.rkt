#lang racket
(require racket threading advent-of-code)

(define rules (~> (fetch-aoc-input (find-session) 2020 7) (string-split "\n")))

(define (parse-bag-rule rule)
  (define parts (regexp-match #px"^([^ ]+ [^ ]+) bags contain (.*)\\.$" rule))
  (let* ([color (second parts)]
         [children (third parts)]
         [child-matches (regexp-match* #px"(\\d+) ([^,]+ [^,]+) bag" children)])
    (list color
          (for/list ([child child-matches])
            (let ([child-parts (regexp-match #px"^(\\d+) ([^,]+ [^,]+) bag" child)])
              (list (string->number (second child-parts)) (third child-parts)))))))

(define bag-hash
  (for/hash ([rule rules])
    (match-let ([(list color children) (parse-bag-rule rule)])
      (values color children))))

;; bag-hash looks like:
'#hash(("bright white" . ((1 "shiny gold")))
       ("dark olive" . ((3 "faded blue") (4 "dotted black")))
       ("dark orange" . ((3 "bright white") (4 "muted yellow")))
       ("dotted black" . ())
       ("faded blue" . ())
       ("light red" . ((1 "bright white") (2 "muted yellow")))
       ("muted yellow" . ((2 "shiny gold") (9 "faded blue")))
       ("shiny gold" . ((1 "dark olive") (2 "vibrant plum")))
       ("vibrant plum" . ((5 "faded blue") (6 "dotted black"))))

;; contained hash flips the direction
;; values are all the bags that contain the key
(define contained-hash
  (for/hash ([hk (hash-keys bag-hash)])
    (values hk (for/fold ([contained '()]
                          #:result (flatten contained))
                         ([(in-k hv) (in-hash bag-hash)]
                          #:when (and ((compose not false?) (member hk (flatten hv)))
                                      ((compose not empty?) hv)))
                 (cons in-k contained)))))

;; contained-hash looks like
'#hash(("bright white" . ("light red" "dark orange"))
       ("dark olive" . ("shiny gold"))
       ("dark orange" . ())
       ("dotted black" . ("vibrant plum" "dark olive"))
       ("faded blue" . ("vibrant plum" "muted yellow" "dark olive"))
       ("light red" . ())
       ("muted yellow" . ("light red" "dark orange"))
       ("shiny gold" . ("muted yellow" "bright white"))
       ("vibrant plum" . ("shiny gold")))

;; part 1
;; now we need to recursively find all the bags "above" shiny gold
(~>
 (let rc ([bag (hash-ref contained-hash "vibrant plum")])
   (match bag
     ['() '()]
     [_
      (append
       (map (compose rc (curry hash-ref contained-hash)) bag) bag)]))
 flatten
 remove-duplicates
 length)

;; part 2
(let rc ([sub-bags (hash-ref bag-hash "shiny gold")])
  (match sub-bags
    ['() 0]
    [_
     (apply + (map (λ (bag)
                     (match-let ([(list bcount color) bag])
                       (+ bcount (* bcount (rc (hash-ref bag-hash color))))))
                   sub-bags))]))
