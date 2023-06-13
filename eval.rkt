#lang typed/racket

(provide (all-defined-out))

(require "expr.rkt"
         "val.rkt")

(: inst (-> Clos Val Val))
(define (inst cl v)
  (match cl
    [(cls p e rho)
     (evaluate e (tupvar rho p v))]
    [(cmp f c)
     (inst f (vcon c v))]
    [(cval v)
     v]))

(: app (-> Val Val Val))
(define (app u v)
  (match* (u v)
    [((vlam f) v)
     (inst f v)]
    [((vfun (sclos es rho)) (vcon c v))
     (let [(res : (Option Expr) 
                (hash-ref es c (lambda () #f)))]
       (if res
        (app (evaluate res rho) v)
        (error (format "error in app\n\n\t~a\n\n\t~a"
                       u v))))]
    [((vfun s) (vnt k))
     (vnt (ntfun s k))]))

(: vfst (-> Val Val))
(define (vfst v)
  (match v 
    [(vpair u _)
     u]
    [(vnt k)
     (vnt (ntfst k))]
    [_ (error (format "error in vfst:\n\n\t~a"
                      v))]))

(: vsnd (-> Val Val))
(define (vsnd v)
  (match v 
    [(vpair _ u)
     u]
    [(vnt k)
     (vnt (ntsnd k))]
    [_ (error (format "error in vsnd:\n\n\t~a"
                      v))]))

(: get-rho (-> Telescope String Val))
(define (get-rho rho x)
  (match rho 
    [(tupvar rho* p v)
     (if (in-pat? x p)
       (pat-proj p x v)
       (get-rho rho* x))]
    [(tupdec rho* (ddec p _ e))
     (if (in-pat? x p)
       (pat-proj p x (evaluate e rho*))
       (get-rho rho* x))]))

(: pat-proj (-> Patt String Val Val))
(define (pat-proj p x v)
  (match p 
    [(pvar y) #:when (equal? x y)
              v]
    [(ppair p1 _) #:when (in-pat? x p1)
                  (pat-proj p1 x (vfst v))]
    [(ppair _ p2) #:when (in-pat? x p2)
                  (pat-proj p2 x (vsnd v))]
    [_ (error (format "error in pat-proj:\n\n\t~a\n\n\t~a\n\n\t~v"
                      p x v))]))

(: in-pat? (-> String Patt Boolean))
(define (in-pat? x p)
  (match p 
    [(pvar y)
     (equal? x y)]
    [(ppair p1 p2)
     (or (in-pat? x p1)
         (in-pat? x p2))]
    [_ #f]))

(: evaluate (-> Expr Telescope Val))
(define (evaluate e r)
  (match e 
    [(evar x)
     (get-rho r x)]
    [(eset)
     (vset)]
    [(edecl d e)
     (evaluate e (tupdec r d))]
    [(elam p e)
     (vlam (cls p e r))]
    [(epi p a b)
     (vpi (evaluate a r) (cls p b r))]
    [(esigma p a b)
     (vsigma (evaluate a r) (cls p b r))]
    [(eone)
     (vone)]
    [(eunit)
     (vunit)]
    [(efst e)
     (vfst (evaluate e r))]
    [(esnd e)
     (vsnd (evaluate e r))]
    [(eapp rator rand)
     (app (evaluate rator r) (evaluate rand r))]
    [(etuple e1 e2)
     (vpair (evaluate e1 r) (evaluate e2 r))]
    [(econ c e1)
     (vcon c (evaluate e1 r))]
    [(esum cas)
     (vsum (sclos cas r))]
    [(efun ces)
     (vfun (sclos ces r))]))

