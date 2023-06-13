#lang typed/racket/shallow

(provide (all-defined-out))

(define-type Expr 
  (U elam eset epi esigma eone eunit 
     econ esum efun efst esnd eapp 
     evar evoid edecl etuple))

(struct elam 
  ([patt : Patt]
   [body : Expr])
  #:transparent)

(struct eset 
  () #:transparent)

(struct etuple 
  ([lhs : Expr]
   [rhs : Expr]) #:transparent)

(struct epi 
  ([pat : Patt]
   [type : Expr]
   [body : Expr]) #:transparent)

(struct esigma
  ([pat : Patt]
   [type : Expr]
   [body : Expr]) #:transparent)

(struct eone () #:transparent)

(struct eunit () #:transparent)

(struct econ 
  ([name : Symbol]
   [body : Expr]) #:transparent)

(struct esum 
  ([branch : Branch]) #:transparent)

(struct efun 
  ([branch : Branch]) #:transparent)

(struct efst 
  ([exp : Expr]) #:transparent)

(struct esnd 
  ([exp : Expr]) #:transparent)

(struct eapp 
  ([rator : Expr]
   [rand : Expr]) #:transparent)

(struct evar 
  ([name : String]) #:transparent)

(struct evoid () #:transparent)

(struct edecl 
  ([decl : Decl]
   [body : Expr]) #:transparent)

(struct ddec 
  ([patt : Patt]
   [type : Expr]
   [body : Expr]) #:transparent)

(struct drec
  ([patt : Patt]
   [type : Expr]
   [body : Expr]) #:transparent)

(define-type Decl (U ddec drec))

(struct ppair 
  ([lhs : Patt]
   [rhs : Patt]) #:transparent)

(struct pvar 
  ([name : String]) #:transparent)

(struct pwild 
  () #:transparent)

(define-type Patt (U pvar ppair pwild))

(define-type Branch (Immutable-HashTable Symbol Expr))
