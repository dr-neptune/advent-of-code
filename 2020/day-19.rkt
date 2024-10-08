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

(define (combine lists-of-strings)
  (if (null? lists-of-strings)
      '("")
      (apply append
             (map (λ (str)
                    (map (λ (suffix)
                           (string-append str suffix))
                         (combine (cdr lists-of-strings))))
                  (car lists-of-strings)))))

(define parsed-rules (format-rules rules))

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

;; part 2
(define 42-messages (get-messages "42"))
(define 31-messages (get-messages "31"))

(define (split-into lst size)
  (if (< (length lst) size)
      '()
      (cons (take lst size) (split-into (drop lst size) size))))

(define (check-str str)
  (let* ([chunks (split-into (string->list str) 8)]
         [total (length chunks)]
         [count-matches (λ (chunks matches)
                          (let loop ([chunks chunks] [cnt 0])
                            (cond [(empty? chunks) cnt]
                                  [(member (apply string (car chunks)) matches)
                                   (loop (cdr chunks) (add1 cnt))]
                                  [else cnt])))]
         [count-42 (count-matches chunks 42-messages)]
         [count-31 (count-matches (reverse chunks) 31-messages)]
         [valid
          (and
           (>= count-31 1)
           (> count-42 count-31)
           (= (+ count-42 count-31) total)
           (for/and ([chunk (in-list (take chunks (- total count-31)))])
             (member (apply string chunk) 42-messages)))])
    valid))

(length (filter identity (map check-str messages)))
