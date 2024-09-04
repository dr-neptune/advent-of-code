#lang racket
(require racket threading advent-of-code)

(define rules (~> (fetch-aoc-input (find-session) 2020 7) (string-split "\n")))

(define rules
  (~> "
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
      (string-split "\n")))

#|

idea

for each bag rule, we identify which bags the sub bags can hold until we hit no other bags
we make dags for each bag type

so for input, we have

light red -> (bright white -> (1 shiny gold -> (+ (1 dark olive -> (+ (3 faded blue bags -> NULL) (4 dotted black bags -> NULL))) (2 vibrant plum))))

maybe we can take advantage of lisp here

line input ->
"light red bags contain 1 bright white bag, 2 muted yellow bags."
A = light red B = bright white C = muted yellow
D = shiny gold
E = dark olive
(A contains (list B C))
(B contains (list D))
->
(A contains (list (B contains (list (D contains (list E )))) C))
then backfill B C until B / C is null

(list color number children)

|#

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


;; now we have a hash that looks like:
'#hash(("bright white" . ((1 "shiny gold")))
       ("dark olive" . ((3 "faded blue") (4 "dotted black")))
       ("dark orange" . ((3 "bright white") (4 "muted yellow")))
       ("dotted black" . ())
       ("faded blue" . ())
       ("light red" . ((1 "bright white") (2 "muted yellow")))
       ("muted yellow" . ((2 "shiny gold") (9 "faded blue")))
       ("shiny gold" . ((1 "dark olive") (2 "vibrant plum")))
       ("vibrant plum" . ((5 "faded blue") (6 "dotted black"))))


;; idea to get contained-by hash
;; for each key
;; get all keys for which the key is a value
(define contained-hash
  (for/hash ([hk (hash-keys bag-hash)])
    (values hk (for/fold ([contained '()]
                          #:result (flatten contained))
                         ([(in-k hv) (in-hash bag-hash)]
                          #:when (and ((compose not false?) (member hk (flatten hv)))
                                      ((compose not empty?) hv)))
                 (cons in-k contained)))))

;; workinonit here

(for/fold ([contained '()])
          ([(hk hv) (in-hash bag-hash)]
           #:when (and ((compose not empty?) hv)
                       ((compose not false?) (member "bright white" (flatten hv)))))
  (cons hk contained))


'#hash(("bright white" . ("dark orange" "light red"))
       ("dark olive" . ("shiny gold"))
       ("dotted black" . ("vibrant plum" "dark olive"))
       ("faded blue" . ("vibrant plum" "dark olive" "muted yellow"))
       ("muted yellow" . ("dark orange" "light red"))
       ("shiny gold" . ("muted yellow" "bright white"))
       ("vibrant plum" . ("shiny gold")))



#|

for each set of values, do the following:

if you find shiny gold, stop
if you find (), stop

otherwise add the number and replace the color with the value in bag-hash

|#

(let ([bag-ref (hash-ref bag-hash "vibrant plum")])
  (map (compose (curry hash-ref bag-hash)
                (λ (val)
                  (match val
                    ['() '()]
                    ["shiny gold" "shiny gold"]
                    [_ val]))
                second) bag-ref))



(let check-bag ([inner-bags (hash-values bag-hash)])
  (displayln (format "inner bags: ~a" inner-bags))
  (for/list ([inner-bag inner-bags])
    (let ([bag-ref inner-bag])
      (map (compose (curry hash-ref bag-hash)
                    (λ (val)
                      (displayln (format "val: ~a" val))
                      (match val
                        ['() '()]
                        ["shiny gold" "shiny gold"]
                        [_ (map check-bag (hash-ref bag-hash val))]))
                    second) bag-ref))))


(define inter (filter
 (λ (v)
   (match v
     ['() #f]
     [(list "shiny gold" _ ...) #f]
     [_ #t]))
 (let check-bag ([inner-bags (hash-values bag-hash)])
   (displayln (format "inner-bags: ~a" inner-bags))
   (for/list ([inner-bag inner-bags])
     (displayln (format "inner bag: ~a" inner-bag))
     (map (compose
           (λ (val)
             (displayln (format "val: ~a" val))
             (match val
               ['() '()]
               ["shiny gold" "shiny gold"]
               [_ (hash-ref bag-hash val)]))
           second)
          inner-bag)))))

(define (flatten/deep ls)
  (cond [(empty? ls) '()]
        [(list? (first ls))
         (append (flatten/deep (first ls))
                 (flatten/deep (rest ls)))]
        [else (cons (first ls) (flatten/deep (rest ls)))]))

(count (curry equal? "shiny gold") (flatten/deep inter))







;; someone else's solution
(define BAG-RULES rules)
(define OUR-BAG "shiny gold")

(define bag-rules-parsed
  (for/list ([bag BAG-RULES])
    (let* ([rule (string-split bag " bags contain ")]
           [bag-containing (car rule)]
           [bag-contained
            (for/list ([contained (cdr rule)])
              (filter non-empty-string?
                      (map (compose string-trim
                                    (λ (s) (string-trim s "bag"))
                                    (λ (s) (string-trim s "bags")))
                           (regexp-split #rx"\\.|," contained))))])
      (list bag-containing (car bag-contained)))))


'(("light red" ("1 bright white" "2 muted yellow"))
  ("dark orange" ("3 bright white" "4 muted yellow"))
  ("bright white" ("1 shiny gold"))
  ("muted yellow" ("2 shiny gold" "9 faded blue"))
  ("shiny gold" ("1 dark olive" "2 vibrant plum"))
  ("dark olive" ("3 faded blue" "4 dotted black"))
  ("vibrant plum" ("5 faded blue" "6 dotted black"))
  ("faded blue" ("no other"))
  ("dotted black" ("no other")))

;; make a hash in which {color: [colors that contain it]}
(define rules-hash
  (for/fold ([contained-to-containing (hash)])
            ([rule bag-rules-parsed])
    (define (add-loop contained-rules)
      (if (or (empty? contained-rules)
              (not (non-empty-string? (car contained-rules)))
              (string=? (car contained-rules) "no other"))
          contained-to-containing
          (let ([loop-acc (add-loop (cdr contained-rules))]
                [val (substring (car contained-rules) 2)])
            (hash-set loop-acc
                      val
                      (cons (car rule) (hash-ref loop-acc val `()))))))
    (add-loop (car (cdr rule)))))

'#hash(("bright white" . ("dark orange" "light red"))
       ("dark olive" . ("shiny gold"))
       ("dotted black" . ("vibrant plum" "dark olive"))
       ("faded blue" . ("vibrant plum" "dark olive" "muted yellow"))
       ("muted yellow" . ("dark orange" "light red"))
       ("shiny gold" . ("muted yellow" "bright white"))
       ("vibrant plum" . ("shiny gold")))

;; start with shiny gold, then for values in shiny gold
;; for each val in values, recurse and get the larger list
;; build a set
(define (get-contained item)
  (let ([item-deps (hash-ref rules-hash item (set))])
    (displayln item-deps)
    (if (empty? item-deps)
        (set)
        (for/fold ([deps (set)])
                  ([dep item-deps])
          (set-add (set-union deps (get-contained dep)) dep)))))


(set-count
 (get-contained OUR-BAG))


;; part 2
(define (to-count-and-item s default-value)
  (let ([n (string->number (substring s 0 1))])
    (if n
        (values n (substring s 2))
        (values default-value s))))

(define (get-inside item-with-possible-number)
  (let-values ([(item-count item) (to-count-and-item item-with-possible-number 1)])
    (for/sum ([rule bag-rules-parsed])
      (if (or
           (string=? item "no other")
           (not (string=? item (car rule))))
          0
          (*
           item-count
           (+ (apply + (map (compose (λ (x _) x)
                                     (λ (s) (to-count-and-item s 0)))
                            (car (cdr rule))))
              (apply + (map get-inside (car (cdr rule))))))))))

(get-inside OUR-BAG)
