#lang racket
(require racket (only-in srfi/1 map))

#|

idea

br00t force

make a new letter generator that increases the next letter in a loop
keep the passwords that fit the pattern
input the most recent and increment until you get a winner

|#

(define (split-into lst size)
  (if (< (length lst) size)
      '()
      (cons (take lst size) (split-into (drop lst size) size))))

;; increase numbers using a base 26 count
(define (char->index/b26 c)
  "Convert a lowercase character 'a'-'z' to its corresponding index 1-26."
  (+ 1 (- (char->integer c) (char->integer #\a))))

(define (index->char/b26 i)
  "Convert an index 1-26 back to its corresponding lowercase character 'a'-'z'."
  (integer->char (+ (- i 1) (char->integer #\a))))

;; Converts a string to a base-26 number using 1-based indexing
(define (string->number/b26 s)
  "Convert a string of lowercase letters to its corresponding base-26 number."
  (foldl (lambda (c acc)
           (+ (* acc 26) (char->index/b26 c)))
         0
         (string->list s)))

;; Converts a base-26 number back to a string using 1-based indexing
(define (number->string/b26 n)
  "Convert a base-26 number to its corresponding string of lowercase letters."
  (define (helper num)
    (if (<= num 26)
        (list (index->char/b26 num))
        (append (helper (floor (/ (- num 1) 26)))
                (list (index->char/b26 (+ (modulo (- num 1) 26) 1))))))
  (if (= n 0)
      ""
      (list->string (helper n))))

;; The main function to get the next string in sequence
(define (next-string s)
  "Given a string of lowercase letters, return the next string in the sequence."
  (number->string/b26 (+ 1 (string->number/b26 s))))

;; pattern matching to check valid pw
;; contains i o l
(define (contains-i-o-l? str)
  (andmap (compose not (curry string-contains? str)) '("l" "i" "o")))

;; 3 in a row
(define (check-increasing-sequence str)
  (let* ([vals (map char->index/b26 (string->list str))]
         [val-diffs (map - vals (rest vals))])
    (for/fold ([seq-count 0]
               [max-count 0]
               #:result (>= max-count 3))
              ([val val-diffs])
      (if (equal? -1 val)
          (values (add1 seq-count)
                  (max max-count (add1 seq-count)))
          (values 0 max-count)))))

;; check at least 2 pairs
(define (two-pair? str)
  (let ([str-ls (string->list str)]
        [eq-pair? (compose
                   (curry map (curry apply equal?))
                   (curryr split-into 2))]
        [true? (compose not false?)])
    (ormap (curryr >= 2) (list (count true? (eq-pair? str-ls)) (count true? (eq-pair? (rest str-ls)))))))


(let loop ([pw "hepxcrrq"]
           ;; [pw "abcdefgh"]
           ;; [pw "ghijklmn"]
           )
  (let* ([vals (map char->index/b26 (string->list pw))]
         [val-diffs (map - vals (rest vals))])
    (if (and (check-increasing-sequence pw)
             (two-pair? pw)
             (contains-i-o-l? pw))
        pw
        (loop (next-string pw)))))

;; got heqqrstt
;; not correct :/
