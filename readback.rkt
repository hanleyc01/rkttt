#lang typed/racket

(provide (all-defined-out))

(require "val.rkt"
         "normal.rkt"
         "eval.rkt")

(: gen-v (-> Integer Val))
(define (gen-v k)
  (vnt (ntgen k)))

(: read-back-val (-> Integer Val Nm))
(define (read-back-val k v*)
  (match v*
    [(vlam f)
     (nmlam k (read-back-val (add1 k) (inst f (gen-v k))))]
    [(vpair u v)
     (nmpair (read-back-val k u) (read-back-val k v))]
    [(vcon c v)
     (nmcon c (read-back-val k v))]
    [(vunit)
     (nmunit)]
    [(vset)
     (nmset)]
    [(vpi t g)
     (nmpi (read-back-val k t) 
           k 
           (read-back-val (add1 k) 
                          (inst g (gen-v k))))]
    [(vsigma t g)
     (nmsigma (read-back-val k t)
              k
              (read-back-val (add1 k)
                             (inst g (gen-v k))))]
    [(vone)
     (nmone)]
    [(vfun (sclos s rho))
     (nmfun (cons s (read-back-tele k rho)))]
    [(vsum (sclos s rho))
     (nmsum (cons s (read-back-tele k rho)))]
    [(vnt l)
     (nmneut (read-back-neut k l))]))

(: read-back-tele (-> Integer Telescope NmTelescope))
(define (read-back-tele k rho)
  (match rho 
    [(tnil)
     (nmtnil)]
    [(tupvar rho* p v)
     (nmtupvar (read-back-tele k rho*)
               p 
               (read-back-val k v))]
    [(tupdec rho* d)
     (nmtupdec (read-back-tele k rho*)
               d)]))

(: read-back-neut (-> Integer Neut NmNt))
(define (read-back-neut k n)
  (match n 
    [(ntgen j)
     (nmntgen j)]
    [(ntapp rator rand)
     (nmntapp (read-back-neut k rator)
              (read-back-val k rand))]
    [(ntfst m)
     (nmntfst (read-back-neut k m))]
    [(ntsnd m)
     (nmntsnd (read-back-neut k m))]
    [(ntfun (cons s rho) m)
     (nmntfun (cons s (read-back-tele k rho))
              (read-back-neut k m))]))
