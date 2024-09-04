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


;; part 2
;; idea


(let rc ([bags (hash-ref bag-hash "shiny gold")])
  (displayln (format "bags: ~a" bags))
  (match bags
    ['() (list 1)]
    [_
     (map (λ (bag)
            (+ (first bag)
               (* (first bag)
                  (apply + (rc (hash-ref bag-hash (second bag)))))))
          bags)]))


'((1 (3 . 1) (4 . 1)) (2 (5 . 1) (6 . 1)))



(let rc ([bags (hash-ref bag-hash "shiny gold")])
  (displayln (format "bags: ~a" bags))
  (match bags
    ['() (list 1)]
    ['(a b) (apply + bags)]
    [_
     (map
      (λ (bag)
        (displayln (format "interior bag: ~a" bag))
        (* (first bag)
           (rc (hash-ref bag-hash (second bag)))))
      bags)]))

 (hash-ref bag-hash "shiny gold")


(let rc ([bags (hash-ref bag-hash "dark olive")])
  (displayln (format "bags: ~a" bags))
  (match bags
    ['() 1]
    [_
     (let rec ([bag bags])
       (cond [(empty? bag) 0]
             [else
              (map (λ (val)
                     (+ (first val) (rec (hash-ref bag-hash (second val)))))
                   bag)
              ]))]))


(let rc ([bags (hash-ref bag-hash "shiny gold")])
  (displayln (format "bags: ~a" bags))
  (match bags
    ['() 1]
    [_
     (let rec ([bag bags])
       (cond [(empty? bag) 0]
             [else
              (map (λ (val)
                     (+ (first val) (rec (hash-ref bag-hash (second val)))))
                   bag)
              ]))]))

(define (check-leaf bag)
  (displayln (format "bag: ~a" bag))
  (if (empty? (hash-ref bag-hash (second bag)))
      (first bag)
      (map (λ (v)
             (displayln v)
             (* (first v)
                (check-leaf (hash-ref bag-hash (second v)))))
           bag)))

(check-leaf '(4 "dark olive"))

#|

1 dark olive
- 3 faded blue
- 4 dotted black

(+ 1 (* 3 1) (* 4 1))

1 * (7) + 2 * (11) + 1 + 2

2 vibrant plum
- 5 faded blue
- 6 dotted black

what's a dumber way to do this?
if hash-ref color is '() return just the above number?

|#

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
;; '#hash(("bright white" . ("light red" "dark orange"))
;;        ("dark olive" . ("shiny gold"))
;;        ("dark orange" . ())
;;        ("dotted black" . ("vibrant plum" "dark olive"))
;;        ("faded blue" . ("vibrant plum" "muted yellow" "dark olive"))
;;        ("light red" . ())
;;        ("muted yellow" . ("light red" "dark orange"))
;;        ("shiny gold" . ("muted yellow" "bright white"))
;;        ("vibrant plum" . ("shiny gold")))

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
