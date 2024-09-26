#lang racket
(require racket threading advent-of-code
         infix-prefix)

(define hw (~> (fetch-aoc-input (find-session) 2020 18)
               (string-split "\n")))

(define-infix->prefix-parser inf-parse * +)

(define (parse-infix-string expr)
  (~> expr (string-append "(" _ ")")
      open-input-string
      read
      inf-parse))


(define hw '("1 + 2 * 3 + 4 * 5 + 6"))
(define hw '("1 + (2 * 3) + (4 * (5 + 6))"))

(eval (parse-infix-string (first hw)))


(define hw '("2 * 3 + (4 * 5)"  ;; becomes 26.
"5 + (8 * 3 + 9 + 3 * 4 * 3)"                          ;; becomes 437.
"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"          ;;  becomes 12240.
"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

(map (compose eval parse-infix-string) hw)


#|

instead of infix->prefix, we need to do the following:
turn an expression into a tree in which each leaf node is
an expression without parens.

|#

(define (try func [else-func identity])
  (λ (x)
    (let ([result (func x)])
      (if result result (else-func x)))))

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define hw (first hw))

(define clean-expr
  (λ~>> string->list
        (filter (λ~> (char=? #\space) not))
        (map (try (λ~>> (make-string 1) string->number)
                  (λ~>> (make-string 1) string->symbol)))))

;; we want to get the inner-most parens,
;; check to make sure there are no more parens
;; then eval the inner expr
;; then check again, and eval,
;; etc
(let rec ([stack '()]
          [exprs (clean-expr hw)])
  (if (empty? exprs)
      stack
      (match-let ([(list fexp sexp ...) exprs])
        (displayln (format "~a\t~a\t\t~a" fexp sexp stack))
        (match fexp
          [(? number?) (rec (cons fexp stack) (rest exprs))]
          [(or '* '+)
           (rec (cons (list fexp (first stack) sexp)
                      (rest stack))
                (rest exprs))]))))

(eval (list (string->symbol "+") 2 3))


(define (is-valid str)
  (define (stack-matcher strls [stack '()])
    (if (not (empty? strls))
        (let ([end-delim (hash-ref *delimiters* (first strls) #f)])
          (cond [(not end-delim) (stack-matcher (rest strls) (append stack (list (first strls))))]
                [(and end-delim (empty? stack)) #f]
                [(equal? (last stack) end-delim)
                 (stack-matcher (rest strls) (drop-right stack 1))]
                [else #f]))
        (empty? stack)))
  (stack-matcher (string->list str)))


(is-valid (string-append "(" hw ")"))

(make-string 1 #\2)
