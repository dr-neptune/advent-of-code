#lang racket
(require racket threading advent-of-code racket/hash)

(define passports (~> (fetch-aoc-input (find-session) 2020 4) (string-split "\n\n")))

(define passports
  (~> "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
      (string-split "\n\n")))


;; idea
;; use regex?
;; find each field, and add them to a hashmap
;; if a value is not found, label it as #f
;; then make a function valid? which gives rules as to validity

#|

can you write me a racket function that takes as input a string and a
list of strings and for each string in the list of strings searches
the string for a pattern that is marked str: value and places it

Can you write me a racket function that takes in a string

|#

;; read in a list of regexps
;; if not found, return key: #f
;; if found, return key: value

(define passport-fields
  '("(byr):([0-9]+)"
    "(iyr):([0-9]+)"
    "(eyr):([0-9]+)"
    "(hgt):([0-9]+)"
    "(hcl):(#[a-z]+)"
    "(ecl):([a-z]+)"
    "(pid):([0-9]+)"
    "(cid):([0-9]+)"))

(define (regexp-match/mult regexps str)
  (map (λ (re)
         (let ([rem (regexp-match re str)])
           (if rem (rest rem) (list re #f))))
       regexps))

(for/list ([passport passports])
  (for/hash ([matches (regexp-match/mult
                       passport-fields
                       passport)])
    (apply values matches)))

;; interesting cases:
;; first inter should be good
;; second inter should be invalid
;; third inter should pass because only cid is false

(first inter)

(filter (λ (vl) (not (string-contains? vl "cid"))) (hash-keys (hash-filter-values (second inter) false?)))

(define (valid? hsh)
  (zero?
   (length
    (filter (λ (vl) (not (string-contains? vl "cid"))) (hash-keys (hash-filter-values hsh false?))))))
