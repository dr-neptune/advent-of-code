#lang racket
(require racket)

(define (write-strings-to-file strings filename)
  (with-output-to-file filename
    (lambda ()
      (for-each (lambda (s)
                  (println s))
                strings))
    #:exists 'replace))

(define input-file "../2015/day-8-input")
(define output-file-adj-bytes "../2015/day-8-pt-2")

(define (sum-byte-strings file-path)
  (for/sum ([fl (file->bytes-lines file-path)])
    (bytes-length fl)))

;; part 1
(define string-lengths (apply + (map string-length (file->list input-file))))
(define bytes-lengths (sum-byte-strings input-file))

(- bytes-lengths string-lengths)

;; part 2
(define adjusted-byte-strings
  (for/list ([ip (for/list ([fl (file->bytes-lines input-file)])
                   (bytes->string/utf-8 fl))])
    ip))

(write-strings-to-file adjusted-byte-strings output-file-adj-bytes)

(define bytes-lengths-2 (sum-byte-strings output-file-adj-bytes))

(- bytes-lengths-2 bytes-lengths)
