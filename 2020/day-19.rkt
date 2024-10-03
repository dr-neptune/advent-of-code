#lang racket
(require racket threading advent-of-code)

(define messages (~> (fetch-aoc-input (find-session) 2020 19)
                     (string-split "\n")))

#|

idea

some kind of rule graph? Each rule makes a DAG

|#
