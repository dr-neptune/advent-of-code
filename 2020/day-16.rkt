#lang racket
(require racket threading advent-of-code
         rebellion/base/range)

(define-struct field (name ranges))

(define (parse-ticket ticket)
  (map string->number (string-split ticket ",")))

(define (parse-field field-str)
  (define split-result (string-split field-str ": "))
  (let ([name (first split-result)]
        [ranges-part (second split-result)])
    (define range-strings (string-split ranges-part " or "))
    (define ranges (map parse-range range-strings))
    (field name ranges)))

(define (parse-range range-str)
  "Parses a range string like '1-3' into a Range object."
  (define split-result (map string->number (string-split range-str "-")))
  (define start (first split-result))
  (define end (second split-result))
  (closed-range start end))

(define ticket-info
  (~>> (fetch-aoc-input (find-session) 2020 16)
       (string-split _ "\n\n")
       (map (λ~> (string-split "\n")))
       ((λ (tri)
          (let ([parse-tickets (curry map parse-ticket)]
                [parse-fields (curry map parse-field)])
            (list (parse-fields (first tri))
                  (parse-tickets (drop (second tri) 1))
                  (parse-tickets (drop (third tri) 1))))))))

;; part 1
(let* ([ranges (first ticket-info)]
       [tickets (third ticket-info)]
       [field-ranges (flatten (map field-ranges (first ticket-info)))]
       [valid-number? (λ (num)
                        (ormap identity (map (λ (range) (range-contains? range num)) field-ranges)))])
  (for*/sum ([ticket tickets]
             [ticket-val ticket]
             #:when (false? (valid-number? ticket-val)))
    ticket-val))

;; part 2
;; find invalid numbers
(define invalid-numbers
  (let* ([ranges (first ticket-info)]
         [tickets (third ticket-info)]
         [field-ranges (flatten (map field-ranges (first ticket-info)))]
         [valid-number? (λ (num)
                          (ormap identity (map (λ (range) (range-contains? range num)) field-ranges)))])
    (for*/list ([ticket tickets]
                [ticket-val ticket]
                #:when (false? (valid-number? ticket-val)))
      ticket-val)))

;; if a ticket contains any invalid value, drop it entirely
(define (list-contains-any? base-list membership-list)
  (ormap (λ (x) (not (not (member x membership-list)))) base-list))

(define (ranges-contains? ranges v) (ormap (λ~> (range-contains? v)) ranges))

(define valid-tickets
  (filter
   (compose not (curryr list-contains-any? invalid-numbers))
   (third ticket-info)))

(define (range-union-contains? ranges v)
  (ormap (λ~> (range-contains? v)) ranges))

(apply set-intersect
       (let* ([fields (first ticket-info)])
         (for/list ([ticket valid-tickets])
           (for/fold ([solns (mutable-set)])
                     ([perm (in-permutations fields)]
                      #:when (andmap (λ (ru t) (range-union-contains? ru t)) (map field-ranges perm) ticket)
                      #:break (equal? (set-count solns) 1))
             (let ([res (map field-name perm)])
               (if (set-empty? solns)
                   res
                   (set-intersect res solns)))))))
