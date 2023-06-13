#lang typed/racket

;; this module provides a naive implementation of translating 
;; rkttt -> rkt

(require "expr.rkt")

(: translate-decls (-> (Listof Decl) (Listof (Listof Symbol))))
(define (translate-decls ds)
  (match ds 
    ['() empty]
    [(cons x xs)
     (cons (translate-decl x) (translate-decls xs))]))

(: translate-decl (-> Decl (Listof Symbol)))
(define (translate-decl d)
  (match d 
    [(ddec n _ b)
     `(define ,(translate-patt n) ,(translate-expression b))]
    [(drec n _ b)
     `(define ,(translate-patt n) ,(translate-expression b))]))

(: translate-expression (-> Expr Symbol))
(define (translate-expression e)
  (match e 
    [_ (error "todo")]))

(: translate-patt (-> Patt Symbol))
(define (translate-patt patt)
  (error "undefined"))
