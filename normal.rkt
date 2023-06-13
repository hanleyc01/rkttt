#lang typed/racket

(provide (all-defined-out))

(require "expr.rkt")

(define-type Nm (U nmlam nmpair nmcon nmunit 
                   nmset nmone nmpi nmsigma nmfun 
                   nmsum nmneut))

(struct nmlam 
  ([n : Integer]
   [body : Nm]) #:transparent)

(struct nmpair 
  ([lhs : Nm]
   [rhs : Nm]) #:transparent)

(struct nmcon 
  ([name : Symbol]
   [body : Nm]) #:transparent)

(struct nmunit 
  () #:transparent)

(struct nmset 
  () #:transparent)

(struct nmone 
  () #:transparent)

(struct nmpi 
  ([type : Nm]
   [n : Integer]
   [body : Nm]) #:transparent)

(struct nmsigma
  ([type : Nm]
   [n : Integer]
   [body : Nm]) #:transparent)

(struct nmfun 
  ([clos : NmClos]) #:transparent)

(struct nmsum
  ([clos : NmClos]) #:transparent)

(struct nmneut
  ([nt : NmNt]) #:transparent)

(define-type NmNt (U nmntgen
                     nmntapp
                     nmntfst
                     nmntsnd
                     nmntfun))

(struct nmntgen 
  ([n : Integer]))

(struct nmntapp
  ([rator : NmNt]
   [rand : Nm]) #:transparent)

(struct nmntfst
  ([exp : NmNt]) #:transparent)

(struct nmntsnd
  ([exp : NmNt]) #:transparent)

(struct nmntfun 
  ([clos : NmClos]
   [neut : NmNt]) #:transparent)

(define-type NmTelescope (U nmtupvar nmtupdec nmtnil))

(struct nmtnil () #:transparent)

(struct nmtupvar 
  ([rho : NmTelescope]
   [patt : Patt]
   [nm : Nm]) #:transparent)

(struct nmtupdec
  ([rho : NmTelescope]
   [decl : Decl]))

(define-type NmClos (Pairof Branch NmTelescope))
