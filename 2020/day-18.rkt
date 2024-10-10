#lang racket
(require racket threading advent-of-code)

(define hw (~> (fetch-aoc-input (find-session) 2020 18)
               (string-split "\n")))

(define (try func [else-func identity])
  (λ (x)
    (let ([result (func x)])
      (if result result (else-func x)))))

(define (clean-simple-expr expr)
  (map (lambda (token)
         (if (regexp-match? #px"^[0-9]+$" token)
             (string->number token)
             (string->symbol token)))
       (regexp-match* #px"\\d+|[+*]" expr)))

(define (eval-simple-expr expr)
  (let ([tokens (clean-simple-expr expr)])
    (let loop ([tokens tokens] [current-result 0] [current-op '+])
      (cond
        [(null? tokens) current-result]
        [(number? (car tokens))
         (let ([num (car tokens)])
           (loop (cdr tokens)
                 (case current-op
                   ['+ (+ current-result num)]
                   ['* (* current-result num)]
                   [else (error "Unsupported operator" current-op)])
                 current-op))]
        [(symbol? (car tokens))
         (let ([op (car tokens)])
           (if (or (eq? op '+) (eq? op '*) )
               (loop (cdr tokens) current-result op)
               (error "Unsupported operator" op)))]
        [else
         (error "Invalid token" (car tokens))]))))

(define (find-expressions-without-nested-parens s)
  (let loop ([i 0]
             [stack '()]
             [results '()])
    (if (>= i (string-length s))
        (reverse results)
        (let ([c (string-ref s i)])
          (match c
            [#\( (loop (add1 i) (cons i stack) results)]
            [#\)
             (if (null? stack)
                 (loop (add1 i) stack results)
                 (let* ([start (car stack)]
                        [end i]
                        [expr (substring s (add1 start) end)]
                        [has-inner (or (string-contains? expr "(")
                                       (string-contains? expr ")"))]
                        [evaluation (eval-simple-expr expr)]
                        [new-results (if (and (not has-inner) evaluation)
                                       (cons (list expr (list start end) evaluation) results)
                                       results)]
                        [new-stack (cdr stack)])
                   (loop (add1 i) new-stack new-results)))]
            [_ (loop (add1 i) stack results)])))))

(define (replace-expressions-in-string s exprs)
  (let* ([sorted-exprs (sort exprs (λ (a b) (< (first (second a)) (first (second b)))))]
         [output (open-output-string)]
         [pos 0])
    (for ([expr-info (in-list sorted-exprs)])
      (let* ([expr (first expr-info)]
             [indices (second expr-info)]
             [evaluation (third expr-info)]
             [start (first indices)]
             [end (second indices)])
        ;; Write text before the expression (including the parentheses)
        (write-string s output pos start)
        ;; Write the evaluated value
        (display evaluation output)
        ;; Update pos to after the closing parenthesis
        (set! pos (add1 end))))
    ;; Write the remaining text
    (write-string s output pos (string-length s))
    (get-output-string output)))

(define (evaluate-expression s)
  (let loop ([expr s])
    (if (or (string-contains? expr "(")
            (string-contains? expr ")"))
        ;; Expression contains parentheses, process them
        (let ([exprs (find-expressions-without-nested-parens expr)])
          (if (null? exprs)
              (error "Unbalanced or invalid expression: " expr)
              (let ([new-expr (replace-expressions-in-string expr exprs)])
                (loop new-expr))))
        ;; No parentheses left, evaluate the expression
        (eval-simple-expr expr))))

(define (process-strings strings)
  (map evaluate-expression strings))

(apply + (process-strings hw))

;; part 2
#|

now we want to move operator precedence to set + before *

approach
parse str to sym
move infix -> prefix with set operator precedence
eval

This is relatively well-trodden space in lisp, so there is a library
to convert infix to prefix. Thankfully it supports operator
precedence.

|#
(require infix-prefix)

(define-infix->prefix-parser add-first * +)

(define (string->syntax str)
  (read (open-input-string (string-append "(" str ")"))))

(apply + (map (compose eval add-first string->syntax) hw))
