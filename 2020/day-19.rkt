#lang racket
(require racket threading advent-of-code)

(match-define (list rules messages) (map (λ~> (string-split _ "\n"))
                                         (~> (fetch-aoc-input (find-session) 2020 19)
                                             (string-split "\n\n"))))

(define (format-rules rules)
  (let ([rule-regex #px"^([0-9]+):\\s+(?:(?:\"([^\"]+)\")|([0-9\\s|]+))$"])
    (for/hash ([rule rules])
      (match-let ([(list _ k letter redir) (regexp-match rule-regex rule)])
        (if letter
            (values k letter)
            (if (string-contains? redir "|")
                (values k (map string-split (string-split redir " | ")))
                (values k (list (string-split redir)))))))))

(define parsed-rules (format-rules rules))

(define (combine lists-of-strings)
  (if (null? lists-of-strings)
      '("")
      (apply append
             (map (λ (str)
                    (map (λ (suffix)
                           (string-append str suffix))
                         (combine (cdr lists-of-strings))))
                  (car lists-of-strings)))))

(define (get-messages [start "0"])
  (let rec ([rule-id start])
    (let ([hr (hash-ref parsed-rules rule-id)])
      (match hr
        [(? string?) (list hr)]
        [_ (~>> hr
                (map (λ~>> (map rec) combine))
                (apply append))]))))

(define (valid-messages [start "0"])
  (let ([possible-messages (get-messages start)])
    (~>> messages (filter (λ~> (member _ possible-messages))))))


;; part 1
(length (valid-messages))

#|

part 2
there can be infinite loops?
maybe I can add a counter and stop off any loop once the count hits the max length of a given message

|#

(match-define (list rules messages)
  (map (λ~> (string-split _ "\n"))
       (~> ;; (fetch-aoc-input (find-session) 2020 19)
        "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
           (string-split "\n\n"))))

(define parsed-rules (format-rules rules))

;; (for ([update '(("8" ("42" "8")) ("11" ("42" "11" "31")))])
;;   (set! parsed-rules (hash-update parsed-rules (first update) (λ~> first (cons (list (second update)))))))

#|

before we have n messages
after we have n + m messages

if we can find those values that aren't found in the initial ruleset and
do match the updated rule set, then we have our answer

our rule changes

8 is 42, or 42 followed by 8
or {42}+
if we find what 42 resolves to, we have our "atom" of a pattern

11 is 42 then 31, or 42 11 31

|#
(define base-messages (valid-messages "0"))
(define 42-messages (get-messages "42"))
(define 31-messages (get-messages "42"))

;; so now we need to look for strings in messages that
;; are not in the initial set, and
;; are some combination of many 42-messages
;; the 42 and 31 messages are always 5 digits long

(define (list-partition predicate ls)
  (call-with-values (λ () (partition predicate ls)) list))


(define (split-into ls subls-size)
  (if (<= (length ls) subls-size)
      (list ls)
      (append (list (take ls subls-size))
              (split-into (drop ls subls-size) subls-size))))


(~> ;; "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
 ;; "bbabbbbaabaabba"
 ;"aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
 ;; "bbbbbbbaaaabbbbaaabbabaaa"
 ;; "bbbababbbbaaaaaaaabbababaaababaabab"
;; "ababaaaaaabaaab"
;; "ababaaaaabbbaba"
;; "baabbaaaabbaaaababbaababb"
;; "abbbbabbbbaaaababbbbbbaaaababb"
;; "aaaaabbaabaaaaababaa"
;; "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
 "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
    string->list
    (split-into _ 5)
    (map list->string _)
    (list-partition (λ (s) (member s 42-messages)) _)
    second
    (map (λ (s) (member s 31-messages)) _))

#|

need to handle rule 11, where we have a balanced number of 42 and 31
not getting any hits on just rule 8 though, so something is off

|#
