#lang racket
(require racket threading advent-of-code)

(define strings (~> (fetch-aoc-input (find-session) 2015 5)
                    (string-split "\n")))

(define bad-strings '("ab" "cd" "pq" "xy"))

(define (string-not-contains/list? str bad-ls)
  (for/and ([bad-ele bad-ls])
    (not (string-contains? str bad-ele))))

(define (vowel? letter)
  (define vowels '(#\a #\e #\i #\o #\u))
  ((compose not false?) (member letter vowels)))

(define-syntax (find-pattern stx)
  (syntax-case stx ()
    ;; Case with #:when clause
    [(_ str-ls pattern result condition)
     #'(match str-ls
         [pattern #:when condition result]
         [_ #f])]
    ;; Case without #:when clause
    [(_ str-ls pattern result)
     #'(match str-ls
         [pattern result]
         [_ #f])]))

;; part 1

(define (nice-string str)
  (let ([str-ls (string->list str)])
    (and (find-pattern str-ls
                       (list _ ... v1 _ ... v2 _ ... v3 _ ...)
                       (list v1 v2 v3)
                       (andmap vowel? (list v1 v2 v3)))
         (find-pattern str-ls (list _ ... a a _ ...) (list a))
         (string-not-contains/list? str bad-strings))))

(for/sum ([str strings]
          #:when (nice-string str))
  1)

;; part 2
(define (nice-string str)
  (let ([str-ls (string->list str)])
    (and (find-pattern str-ls (list _ ... a b _ ... a b _ ...) (list a b))
         (find-pattern str-ls (list _ ... a _ a _ ...) (list a)))))

(for/sum ([str strings]
          #:when (nice-string str))
  1)
