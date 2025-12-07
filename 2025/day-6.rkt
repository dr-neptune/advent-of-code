#lang racket
(require racket advent-of-code)

(define homework (fetch-aoc-input (find-session) 2025 6))

;; Part 1 approach: Split each row into whitespace-separated tokens (numbers or operators),
;; transpose rows into columns so the operator (bottom row) becomes the head, then eval/sum.
(define rows
  (for/list ([ln (in-list (string-split homework "\n" #:repeat? #t))])
    (for/list ([tok (in-list (string-split ln #px"\\s+"))])
      (if (regexp-match? #px"^\\d+$" tok)
          (string->number tok)
          (string->symbol tok)))))

;; Group by column, then reverse so the operator (last row) leads each column.
(define columns
  (for/list ([col (in-range (length (first rows)))])
    (reverse (for/list ([row (in-list rows)])
               (list-ref row col)))))

;; part 1
(define ns (make-base-namespace))
(for/sum ([col columns]) (eval col ns))

;; Part 2 approach: Assume a rectangular char grid, transpose to columns, split on
;; all-space columns to find expression groups, strip spaces, collapse digit runs
;; into numbers, pick the operator, and eval each expression.
(define (char->digit char) (- (char->integer char) (char->integer #\0)))
(define (char->symbol ch) (string->symbol (string ch)))
(define (digits->number ds) (for/fold ([n 0]) ([d ds]) (+ (* n 10) d)))
(define (column->number col) (digits->number (filter number? col)))
(define (space-col? col) (andmap (curry eq? #\space) col))

(define (expressions-from s)
  (define lines (string-split s "\n" #:repeat? #t))
  (define columns (apply map list (map string->list lines)))
  (define-values (acc cur)
    (for/fold ([acc '()] [cur '()]) ([col (in-list columns)])
      (if (space-col? col)
          (values (if (null? cur) acc (cons (reverse cur) acc)) '())
          (values acc (cons col cur)))))
  (define column-groups
    (reverse (if (null? cur) acc (cons (reverse cur) acc))))
  (for/list ([grp (in-list column-groups)])
    (define token-cols
      (for/list ([col (in-list grp)])
        (for/list ([ch (in-list col)] #:unless (char=? ch #\space))
          (if (char-numeric? ch) (char->digit ch) (char->symbol ch)))))
    (cons (findf symbol? (apply append token-cols))
          (map column->number token-cols))))

(for/sum ([expr (expressions-from homework)]) (eval expr ns))
