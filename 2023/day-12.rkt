#lang racket
(require racket advent-of-code)

(define condition-records (string-split (fetch-aoc-input (find-session) 2023 12) "\n"))

(define condition-records (string-split "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1" "\n"))

(first condition-records)

#|

idea

???.### 1,1,3

|#

(define (parse-condition-record cr)
  (match-let ([(list grid vals) (string-split cr)])
    (cons (string->list grid) (list (map string->number (string-split vals ","))))))

;; (define (all-equal? ls [char '()])
;;   (if (empty? char)
;;       (andmap (λ (a) (equal? a (car ls))) ls)
;;       (andmap (λ (_) (equal? char (car ls))) ls)))

(define (all-equal? ls [char '()])
  (match char
    ['() (andmap (λ (a) (equal? a (car ls))) ls)]
    [(list a) (andmap (λ (_) (equal? (first char) (car ls))) ls)]
    [(list a ...)
     (andmap (λ (_) ((compose not false? member) (car ls) char)) ls)]))

;; ((compose not false? member) #\# '(#\, #\. #\#))
;; ((compose not false? member) #\# '(#\, #\. #\,))
;; (all-equal? '(#\. #\. #\.) '(#\, #\. #\,))
;; (all-equal? '(#\. #\. #\,) '(#\, #\. #\,))



(define inter (parse-condition-record (first condition-records)))

;; (match-let ([(list grid vals) inter])
;;   (let loop ([cr grid]
;;              [vals vals])
;;     (displayln (format "~a ~a" cr vals))
;;     (cond [(and (empty? vals) (empty? cr)) (begin
;;                                              (displayln "reached endpoint 1")
;;                                              1)]
;;           [(or (empty? vals) (empty? cr)) (begin
;;                                             (displayln "reached endpoint 0")
;;                                             0)]
;;           [(> (first vals) (length cr)) (begin
;;                                           (displayln "reached endpoint 0.2")
;;                                           0)]
;;           [else
;;            (let ([f (first cr)])
;;              (match f
;;                [#\. (loop (rest cr) vals)]
;;                [#\? (cons (loop (cons #\# (rest cr)) vals)
;;                           (loop (cons #\. (rest cr)) vals))]
;;                [#\# (let* ([next-vals (take cr (first vals))]
;;                            [vals-eq? (all-equal? next-vals #\#)])
;;                       (cond [vals-eq? (loop (drop cr (first vals)) (rest vals))]
;;                             [else (loop (rest cr) vals)]))]))])))


(define inter (parse-condition-record (first condition-records)))

(define (end-state grid vals)
  (cond [(and (empty? grid) (empty? vals)) 1]
        [(or (empty? grid) (empty? vals)) 0]
        [(> (first vals) (length grid)) 0]
        [else #f]))

(match-let ([(list grid vals) inter])
  (let loop ([cr grid]
             [vals vals])
    (let ([end-point (end-state cr vals)])
      (if (false? end-point)
          (let ([f (first cr)])
            (match f
              [#\. (loop (rest cr) vals)]
              [#\? (+ (loop (cons #\# (rest cr)) vals)
                      (loop (cons #\. (rest cr)) vals))]
              [#\# (let* ([next-vals (take cr (first vals))]
                          [vals-eq? (all-equal? next-vals '(#\# #\?))])
                     (cond [vals-eq? (loop (drop cr (first vals)) (rest vals))]
                           [else (loop (rest cr) vals)]))]))
          end-point))))


;; (match-let ([(list grid vals) inter])
;;   (let loop ([cr grid]
;;              [vals vals])
;;     (displayln (format "~a ~a" cr vals))
;;     (cond [(or (and (empty? vals)
;;                     (all-equal? cr '(#\.)))
;;                (and (empty? vals) (empty? cr)))
;;            (begin
;;              (displayln "reached endpoint 1")
;;              1)]
;;           [(or (empty? vals)
;;                (empty? cr))
;;            (begin
;;              (displayln "reached endpoint 0")
;;              0)]
;;           [(> (first vals) (length cr)) (begin
;;                                           (displayln "reached endpoint 0.2")
;;                                           0)]
;;           [else
;;            (let ([f (first cr)])
;;              (match f
;;                [#\. (loop (rest cr) vals)]
;;                [#\? (+ (loop (cons #\# (rest cr)) vals)
;;                        (loop (cons #\. (rest cr)) vals))]
;;                [#\# (let* ([next-vals (take cr (first vals))]
;;                            [vals-eq? (all-equal? next-vals '(#\# #\?))])
;;                       (cond [vals-eq? (loop (drop cr (first vals)) (rest vals))]
;;                             [else (loop (rest cr) vals)]))]))])))

(define inter (parse-condition-record (first condition-records)))

(define (end-state grid vals)
  (cond [(and (empty? grid) (empty? vals)) (begin (displayln "exit 1") 1)]
        [(or (empty? grid) (empty? vals)) (begin (displayln "exit 0") 0)]
        [(> (first vals) (length grid)) (begin (displayln "exit 0.2") 0)]
        [else #f]))

(match-let ([(list grid vals) inter])
  (let loop ([cr grid]
             [vals vals])
    (let ([end-point (end-state cr vals)])
      (if (false? end-point)
          (let ([f (first cr)])
            (displayln (format "~a ~a ~a" cr vals f))
            (match f
              [#\. (loop (rest cr) vals)]
              [#\? (cons
                     (loop (cons #\# (rest cr)) vals)
                     (loop (cons #\. (rest cr)) vals))]
              [#\# (let* ([next-vals (take cr (first vals))]
                          [vals-eq? (all-equal? next-vals '(#\# #\?))])
                     (cond [vals-eq? (loop (drop cr (first vals)) (rest vals))]
                           [else (loop (drop cr (first vals)) vals)]))]))
          end-point))))

;; dfs
