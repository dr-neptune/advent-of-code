#lang racket
(require racket threading advent-of-code)

(define hw (~> (fetch-aoc-input (find-session) 2020 18)
               (string-split "\n")))

(define hw '("1 + 2 * 3 + 4 * 5 + 6"))
(define hw '("1 + (2 * 3) + (4 * (5 + 6))"))

(define hw '("2 * 3 + (4 * 5)"  ;; becomes 26.
"5 + (8 * 3 + 9 + 3 * 4 * 3)"                          ;; becomes 437.
"5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"          ;;  becomes 12240.
"((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

(define (try func [else-func identity])
  (λ (x)
    (let ([result (func x)])
      (if result result (else-func x)))))

(define clean-expr
  (λ~>> string->list
        (filter (λ~> (char=? #\space) not))
        (map (try (λ~>> (make-string 1) string->number)
                  (λ~>> (make-string 1) string->symbol)))))

(define (eval-simple-expr expr)
  (let rec ([stack '()] [exprs (clean-expr hw)])
    (match (list exprs stack)
      [(list '() (list a)) a]
      [(list (list fexp sexp ...) (list a ...))
       (match fexp
         [(? number?) (rec (cons fexp stack) (rest exprs))]
         [(or '* '+)
          (rec (cons (eval (list fexp (first stack) (first sexp)))
                     (rest stack))
               (rest (rest exprs)))])])))


;; (define hw "1 + 2 * 3 + 4 * 5 + 6")
;; (eval-simple-expr (clean-expr hw))

(define (match-parentheses hw)
  (let* ([chars (string->list hw)]
         [idxed (map cons (range (string-length hw)) chars)])
    (for/fold ([stack '()] [pairs '()]
               #:result (reverse pairs))
              ([idx idxed])
      (match (cdr idx)
        [#\( (values (cons (car idx) stack) pairs)]
        [#\) (values (rest stack) (cons (cons (car stack) (car idx)) pairs))]
        [_ (values stack pairs)]))))


(define-splicing-for-clause-syntax cross3
  (lambda (stx)
    (syntax-case stx ()
      [(_ n m) #'([n (in-range 3)]
                  #:when #t
                  [m (in-range 3)])])))

(for (#:splice (cross3 n m))
    (println (list n m)))

(for (#:splice (a b) (map cons (range 1 5) (range 2 6)))
  a)

;; Example usage
(define hw "1 + (2 * 3) + (4 * (5 + 6))")

(for/list ([pair (sort-pairs-descending (match-parentheses hw))]
           #:do [(match-define (cons op cl) pair)])
  )

#|

for each pair in the sorted list
get the substring and evaluate it
then make a string with those values
and inject them into the original string
this is essentially a reduction

|#
;; Output: '((4 . 10) (14 . 26) (19 . 25))


(define (sort-pairs-descending pairs)
  (sort pairs
        (lambda (p1 p2)
          (> (car p1) (car p2)))))  ; Sort by opening index descending




;; yippety
;; Function to Sort Pairs Descending

(define (sort-pairs-descending pairs)
  (sort pairs
        (lambda (p1 p2)
          (> (car p1) (car p2)))))  ; Sort by opening index descending

;; Function to Replace Expressions

(define (replace-expressions hw sorted-pairs)
  (foldl
    (lambda (pair acc-hw)
      (define open (car pair))
      (define close (cdr pair))
      ;; Extract the substring inside the current pair of parentheses
      (define inner-str (substring acc-hw (+ open 1) close))
      ;; Clean and parse the expression
      (define expr (clean-expr inner-str))
      ;; Evaluate the expression
      (define val (eval-simple-expr expr))
      ;; Replace the parenthetical expression with its evaluated value
      (string-append
        (substring acc-hw 0 open)
        (number->string val)
        (substring acc-hw (+ close 1))))
    hw
    sorted-pairs))

;; Main Evaluation Function

(define (evaluate-expression hw)
  (let* ([pairs (find-parentheses-pairs hw)]
         [sorted-pairs (sort-pairs-descending pairs)]
         [hw-after-replacements (replace-expressions hw sorted-pairs)]
         [final-expr (clean-expr hw-after-replacements)]
         [result (eval-simple-expr final-expr)])
    result))

;; Example Usage

(define examples
  (list
    "2 * 3 + (4 * 5)"
    "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

(for-each
  (lambda (hw)
    (printf "Expression: ~a\nResult: ~a\n\n" hw (evaluate-expression hw)))
  examples)
