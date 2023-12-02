#lang racket
(require racket advent-of-code threading
         megaparsack megaparsack/text data/monad data/applicative)

(define cmd-history (fetch-aoc-input (find-session) 2022 7))

;; parse commands
;; $ cmd cmd-args -> ('CMD (cmd cmd-args))
(define cmd-arg/p
  (do [initializer <- (char/p #\$)]
      [spacing <- space/p]
      [operator <- (many/p letter/p)]
      [cmd-space <- space/p]
      [operand <- (many/p any-char/p)]
      (pure (list 'CMD (map list->string (list operator operand))))))

;; $ cmd -> ('CMD cmd)
(define cmd-no-arg/p
  (do [initializer <- (char/p #\$)]
      [spacing <- space/p]
      [operator <- (many/p letter/p)]
      (pure (cons 'CMD (map list->string (list operator))))))

;; parse directory output
;; dir a -> ('DIR dir)
(define directory-name/p
  (do [dir-init <- (string/p "dir")]
      [spacing <- space/p]
      [dir-name <- (many/p letter/p)]
      (pure (cons 'DIR (~> dir-name list->string list)))))

;; file-size file-name -> ('FILE file-name file-size)
(define file-info/p
  (do [file-size-int <- integer/p]
      [spacing <- space/p]
      [file-name <- (many/p (or/p letter/p (char/p #\.)))]
      (pure (cons 'FILE (list (list->string file-name) file-size-int)))))

;; tries applying all the different parsers
(define (parse-input input)
  (parse-result!
   (parse-string
    (apply or/p
           (map (λ (p) (try/p p))
                (list cmd-arg/p cmd-no-arg/p directory-name/p file-info/p)))
    input)))

;; get list of inputs in parsed format
(define cmds (map parse-input (string-split cmd-history "\n")))

;; create files
;; set starting location
(current-directory "~/Documents/aoc22/")

;; make temp dir and switch to it
(make-directory (build-path (current-directory) "temp"))
(current-directory (build-path (current-directory) "temp"))

;; make file with a given size in bytes
(define (make-file-with-size fname fsize)
  (system (format "truncate -s ~a ~a" fsize fname)))

(define (dir-exists-else-create dir-path [switch-to #f])
  (if (directory-exists? dir-path)
      (current-directory dir-path)
      (if switch-to
          (begin
            (make-directory dir-path)
            (current-directory dir-path))
          (make-directory dir-path))))

(define (file-exists-else-create file-path file-byte-size)
  (if (file-exists? file-path)
      #f
      (make-file-with-size (~> file-path
                               file-name-from-path
                               path->string) file-byte-size)))

(define (execute-command command)
  (let* ([cmd-type (first command)]
         [cmd-args (rest command)]
         [first-arg (first cmd-args)]
         [base-path ((curry build-path) (current-directory))])
  (cond [(equal? cmd-type 'CMD)
         (if (string? first-arg)  ;; ls, do nothing
             #t
             (let ([dir-name (cadar cmd-args)]) ;; operand
               (cond [(equal? dir-name "/") #t]
                     [(equal? dir-name "..") (current-directory (build-path 'up))]
                     [else (dir-exists-else-create (base-path dir-name))])))]
        [(equal? cmd-type 'DIR)
         (let ([dir-name (first cmd-args)])
           (dir-exists-else-create (base-path dir-name)))]
        [(equal? cmd-type 'FILE)
         (file-exists-else-create (base-path first-arg) (second cmd-args))]
        [else #f])))

;; create file structure
(for-each execute-command cmds)

;; part 1
;; get size of a directory
(define (directory-size directory)
  (~>> directory
       (find-files identity)
       (filter file-exists?)
       (map file-size)
       (foldl + 0)))

(define directory-sizes
  (~>> (find-files identity)
       (filter directory-exists?)
       (map directory-size)))

(~>> directory-sizes
     (filter (λ (s) (< s 100000)))
     (foldl + 0))

;; part 2
(define space-remaining
  (let ([total-space 70000000])
    (- total-space (directory-size (current-directory)))))

(~>> directory-sizes
     (filter (λ (s) (> s (- 30000000 space-remaining))))
     (apply min))
