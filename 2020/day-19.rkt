#lang racket
(require racket threading advent-of-code)

(match-define (list rules messages) (map (λ~> (string-split _ "\n"))
                                         (~> (fetch-aoc-input (find-session) 2020 19)
                                             (string-split "\n\n"))))


#|

idea

some kind of rule graph? Each rule makes a DAG

to match rule 0: 1 2
the text must match rule 1,
so start with a since 1: "1"
then the text after must match rule 2
2: 1 3 | 3 1
so we start with "a"
then match either "ab" OR "ba"

therefore, rule 0 matches either
"aab" or "aba"

so we only need to handle rule 0 for part 1

idea
take each rule,
recurse on the rule number
if the final val is a letter, return it, ow recurse and move forward

lets start by making a hash

|#

(define rules
  (~>
"0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\""
      (string-split _ "\n")))

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

(let rec ([rvals (hash-ref parsed-rules "0")])
  (for/list ([val rvals])
    (println val)
    (match val
      [(? string?) (list val)]
      [_
       (apply append (map (λ (option)
                            (combine (map (λ (subrule) (generate subrule parsed-rules)) option)))))]
      [(? list?) (rec val)]
      [(? (compose number? string->number))
       (rec (hash-ref parsed-rules val))])))


;; yip
(define (combine lists-of-strings)
  (if (null? lists-of-strings)
      '("")
      (apply append
             (map (λ (str)
                    (map (λ (suffix)
                           (string-append str suffix))
                         (combine (cdr lists-of-strings))))
                  (car lists-of-strings)))))


;; Function to generate all possible strings from a given rule
(define (generate rule-id parsed-rules)
  (define rule (hash-ref parsed-rules rule-id))
  (cond
    ;; Terminal rule: return as a single-element list
    [(string? rule) (list rule)]
    ;; Non-terminal rule with options
    [(list? rule)
     (apply append
            (map (λ (option)
                   ;; 'option' is a list of subrule IDs
                   ;; Generate expansions for each subrule
                   (define expansions
                     (map (λ (sr) (generate sr parsed-rules)) option))
                   ;; Combine the expansions to get all possible strings
                   (combine expansions))
                 rule))]
    ;; Unexpected rule format
    [else (error "Unexpected rule format for rule" rule-id)]))

;; Generate the parsed results starting from rule "0"
(define parsed-results (generate "0" parsed-rules))

;; Display the results
(displayln (length parsed-results))

(length (filter (λ~> (member _ parsed-results)) messages))

#|

part 2
there can be infinite loops?
maybe I can add a counter and stop off any loop once the count hits the max length of a given message

|#
