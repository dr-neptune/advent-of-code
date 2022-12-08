#lang racket
(require racket advent-of-code threading
         megaparsack megaparsack/text data/monad data/applicative
         try-catch)

;; idea
;; make the files
;; then get an rglob of directory size
;; then sort > and take everything up to 100k
;; then sum

(define test-cmds #<<"
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"
)
test-cmds



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

;; get list of inputs
(define cmds (map parse-input (string-split test-cmds "\n")))

;; output of cmds
'((CMD ("cd" "/"))
  (CMD "ls")
  (DIR "a")
  (FILE "b.txt" 14848514)
  (FILE "c.dat" 8504156)
  (DIR "d")
  (CMD ("cd" "a"))
  (CMD "ls")
  (DIR "e")
  (FILE "f" 29116)
  (FILE "g" 2557)
  (FILE "h.lst" 62596)
  (CMD ("cd" "e"))
  (CMD "ls")
  (FILE "i" 584)
  (CMD ("cd" ".."))
  (CMD ("cd" ".."))
  (CMD ("cd" "d"))
  (CMD "ls")
  (FILE "j" 4060174)
  (FILE "d.log" 8033020)
  (FILE "d.ext" 5626152)
  (FILE "k" 7214296))

;; create files
