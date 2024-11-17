#lang racket
(require racket threading file/md5)

(define secret-key "yzbqklnj")

#|

idea
we need to find a number to combine with our secret key such that
appending this number to the key and pushing it through md5
will lead to 5 leading zeros
|#

;; brute force
(define (leading-md5-zeroes? num secret-key [num-leading-zeroes 5])
  (~> secret-key
      (string-append (number->string num))
      md5
      bytes->string/utf-8
      string->list
      (take num-leading-zeroes)
      (equal? (make-list num-leading-zeroes #\0))))

;; pt 1
(for/first ([num (in-naturals)]
            #:when (leading-md5-zeroes? num secret-key))
  num)

;; pt 2
(for/first ([num (in-naturals)]
            #:when (leading-md5-zeroes? num secret-key 6))
  num)
