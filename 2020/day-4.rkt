#lang racket
(require racket threading advent-of-code)

(define passports (~> (fetch-aoc-input (find-session) 2020 4) (string-split "\n\n")))

;; part 1
(define structured-passports
  (for/list ([passport passports])
    (for/hash ([split-passport (string-split passport)])
      (apply values (string-split split-passport ":")))))

(define (check-all-fields-exist ht)
  (define passport-fields
    (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))

  (set-empty?
   (apply set-symmetric-difference
          (map (curryr set-remove "cid")
               (list passport-fields (apply set (hash-keys ht)))))))

(define (validate-fields ht)
  (define (validate-number num low high)
    (let ([num (string->number num)])
      (and (number? num) (<= low num high))))

  (define (validate-regex value pattern)
    (regexp-match? pattern value))

  (define (validate name value)
    (match name
      ["byr" (validate-number value 1920 2002)]
      ["iyr" (validate-number value 2010 2020)]
      ["eyr" (validate-number value 2020 2030)]
      ["hgt" (cond
               [(regexp-match? #px"^([0-9]+)cm$" value)
                (validate-number (second (regexp-match #px"^([0-9]+)cm$" value)) 150 193)]
               [(regexp-match? #px"^([0-9]+)in$" value)
                (validate-number (second (regexp-match #px"^([0-9]+)in$" value)) 59 76)]
               [else #f])]
      ["hcl" (validate-regex value #px"^#[0-9a-f]{6}$")]
      ["ecl" (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))]
      ["pid" (validate-regex value #px"^[0-9]{9}$")]
      ;; "cid" is ignored, so always valid
      ["cid" #t]
      [_ #f]))

  (andmap
   (lambda (key)
     (validate key (hash-ref ht key "")))
   '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))

(define (valid? ht validators)
  (andmap (λ (fn) (fn ht)) validators))

;; part 1
(for/sum ([passport structured-passports]
          #:when (valid? passport (list check-all-fields-exist)))
  1)

;; part 2
(for/sum ([passport structured-passports]
          #:when (valid? passport (list check-all-fields-exist validate-fields)))
  1)
