#lang racket

(require "parsing.rkt"
         "expr.rkt"
         "val.rkt"
         "tc.rkt")

(define fart 1)

(define (arrange xs)
  (foldr (lambda (x y) (edecl x y)) (eone) xs))

(define (typecheck in-file)
  (if (empty? in-file)
    (displayln "USAGE: please provide filepath to compile :3")
    (letrec ([inc (open-input-file in-file)]
             [res (parse inc)]
             [arranged (arrange res)])
      (begin 
        (when debug-mode
          (pretty-display res))
         (check-main debug-mode arranged (vunit))
         (displayln "typecheck complete!")))))


(define debug-mode #f)

(define rkttt 
  (command-line 
    #:usage-help 
    "USAGE: welcome to rkttt, this is an implementation of mini-tt in racket"
    ":3 apologies if it is slow, typed racket is slow, and there's definitely"
    "some optimizations to be done!"
    #:once-each 
    [("-t" "--typecheck") PATH "path to file" (typecheck PATH)]
    [("-d" "--debug") "Display debug information" (set! debug-mode #t)]
    #:args ()
    (void)))
