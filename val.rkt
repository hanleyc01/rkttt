#lang typed/racket

(provide (all-defined-out))

(require "expr.rkt")

(define-type Val (U vlam vpair vcon vunit vset 
                    vone vpi vsigma vfun vsum vnt))

(struct vlam 
  ([clos : Clos]) #:transparent)

(struct vpair 
  ([lhs : Val]
   [rhs : Val]) #:transparent)

(struct vcon 
  ([name : Symbol]
   [val : Val]) #:transparent)

(struct vunit () #:transparent)

(struct vset () #:transparent)

(struct vone () #:transparent)

(struct vpi 
  ([val : Val]
   [clos : Clos]) #:transparent)

(struct vsigma 
  ([val : Val]
   [clos : Clos]) #:transparent)

(struct vfun 
  ([clos : SClos]) #:transparent)

(struct vsum 
  ([clos : SClos]) #:transparent)

(struct vnt 
  ([nt : Neut]) #:transparent)

(define-type Neut (U ntgen ntapp ntfst ntsnd ntfun))

(struct ntgen 
  ([n : Integer]) #:transparent)

(struct ntapp 
  ([rator : Neut]
   [rand : Val]) #:transparent)

(struct ntfst 
  ([nt : Neut]) #:transparent)

(struct ntsnd 
  ([nt : Neut]) #:transparent)

(struct ntfun 
  ([clos : SClos]
   [nt : Neut]) #:transparent)

(define-type Clos (U cls cmp cval))

(struct cls
  ([patt : Patt]
   [exp : Expr]
   [rho : Telescope]) #:transparent)

(struct cmp 
  ([clos : Clos]
   [name : Symbol]) #:transparent)

(struct cval 
  ([val : Val]) #:transparent)

(define-type Telescope (U tnil tupvar tupdec))

(define-type SClos sclos)

(struct sclos 
  ([branch : Branch]
   [rho : Telescope]) #:transparent)

(struct tnil () #:transparent)

(struct tupvar 
  ([rho : Telescope]
   [patt : Patt]
   [val : Val]) #:transparent)

(struct tupdec 
  ([rho : Telescope]
   [decl : Decl]) #:transparent)
