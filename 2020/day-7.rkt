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

#|

for each set of values, do the following:

if you find shiny gold, stop
if you find (), stop

otherwise add the number and replace the color with the value in bag-hash

|#

(first bag-hash)
