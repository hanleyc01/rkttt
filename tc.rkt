#lang typed/racket/shallow

(provide check-main)

(require "val.rkt"
         "eval.rkt"
         "expr.rkt"
         "readback.rkt")

(define-type Gamma (Immutable-HashTable String Val))

(: *debug-mode* (Boxof Boolean))
(define *debug-mode* (box #f))

(: up-g (-> Gamma Patt Val Val Gamma)) 
(define (up-g gamma p t v)
  (when (unbox *debug-mode*)
  (displayln (format "up-g with:\n\t~a\n\t~a\n\t~a:\n\t~a"
                     gamma p t v)))
  (match* (p t)
    [((pwild) _)
     gamma]
    [((pvar x) t)
     (hash-set gamma x t)]
    [((ppair p1 p2) (vsigma t g))
     (let ([gamma* (up-g gamma p1 t (vfst v))])
       (up-g gamma* p2 (inst g (vfst v)) (vsnd v)))]))

(: check-type (-> Integer Telescope Gamma Expr Void))
(define (check-type k rho gamma e)
  (when (unbox *debug-mode*)
  (displayln (format "check-type with:\n\t~a\n\t~a\n\t~a\n\t~a" k rho gamma e)))
  (match e
    [(epi p a b)
     (begin (check-type k rho gamma a)
            (define gamma* (up-g gamma p (evaluate a rho) (gen-v k)))
            (check-type (add1 k) (tupvar rho p (gen-v k)) gamma* b))]
    [(esigma p a b)
     (begin (check-type k rho gamma a)
            (define gamma* (up-g gamma p (evaluate a rho) (gen-v k)))
            (check-type (add1 k) (tupvar rho p (gen-v k)) gamma* b))]
    [(eset) (void)]
    [(eunit) (void)]
    [(evoid) (void)]
    [_ (check k rho gamma e (vset))]))

(: check (-> Integer Telescope Gamma Expr Val Void))
(define (check k rho gamma e* t*)
  (when (unbox *debug-mode*)
  (displayln (format "check with:\n\t~a\n\t~a\n\t~a\n\t~a\n\t~a"
                     k rho gamma e* t*)))
  (match* (e* t*)
    [((elam p e) (vpi t g))
     (define gen (gen-v k))
     (define gamma* (up-g gamma p t gen))
     (check (add1 k) (tupvar rho p gen) gamma* e (inst g gen))]
    [((etuple e1 e2) (vsigma t g))
     (check k rho gamma e1 t)
     (check k rho gamma e2 (inst g (evaluate e1 rho)))]
    [((econ c e) (vsum (sclos cas rho*)))
     (define a : (Option Expr) 
       (hash-ref cas c (lambda () #f)))
     (when a 
       (check k rho gamma e (evaluate a rho*)))]
    [((efun ces) (vpi (vsum (sclos cas rho*)) g))
     (define ces* (hash-keys ces))
     (define cas* (hash-keys cas))
     (if (equal? ces* cas*)
       (for* ([x (hash->list ces)]
              [y (hash->list cas)])
         (match* (x y) 
           [((cons c e) (cons _ a))
            (check k rho gamma e 
                   (vpi (evaluate a rho*) 
                        (cmp g c)))]))
       (error (format "branches do not match:\n\n\t~a\n\n\t~a"
                      cas ces)))]
    [((eunit) (vset))
     (void)]
    [((eone) (vunit))
     (void)]
    [((epi p a b) (vset))
     (check k rho gamma a (vset))
     (define gen (gen-v k))
     (define gamma* (up-g gamma p (evaluate a rho) gen))
     (check (add1 k) (tupvar rho p gen) gamma* b (vset))]
    [((esigma p a b) (vset))
     (check k rho gamma (epi p a b) (vset))]
    [((esum cas) (vset))
     (for ([a (hash-values cas)])
       (check k rho gamma a (vset)))]
    [((edecl d e) t)
     (define gamma* (check-decl k rho gamma d))
     (check k (tupdec rho d) gamma* e t)]
    [(_ _)
      (define t** (check-infer k rho gamma e*))
      (when (equal-nf? k t* t**)
        (void))]))

(: check-infer (-> Integer Telescope Gamma Expr Val))
(define (check-infer k rho gamma e*)
  (when (unbox *debug-mode*)
  (displayln (format "check-infer with:~a\n\t~a\n\t~a\n\t~a"
                     k rho gamma e*)))
  (match e*
    [(eset) (vset)]
    [(eunit) (vset)]
    [(eone) (vunit)]
    [(evoid) (vset)]
    [(evar x)
     (let ([res : (Option Val) (hash-ref gamma x #f)])
       (if res
         res 
         (error (format "check-infer error with gamma:\n\n\t~a\n\nand expr:\n\n\t~a"
                        gamma e*))))]
    [(eapp rator rand)
     (define t* (check-infer k rho gamma rator))
     (letrec ([x (ext-pi t*)]
              [t (car x)]
              [g (cdr x)])
       (begin (check k rho gamma rand t)
              (inst g (evaluate rand rho))))]
    [(etuple lhs rhs)
     (define lft (check-infer k rho gamma lhs))
     (define rht (check-infer k rho gamma rhs))
     (vsigma lft (cval rht))]
    [(efst e)
     (define t (check-infer k rho gamma e))
     (letrec ([x (ext-sigma t)]
              [a (car x)])
       a)]
    [(esnd e)
     (define t (check-infer k rho gamma e))
     (letrec ([x (ext-sigma t)]
              [g (cdr x)])
       (inst g (vfst (evaluate e rho))))]
    [_ (error 
         (format 
           (string-append 
             "unable to infer an expression which is not one of the following:"
             "\n\ta projection (car/cdr <expr>)\n\ta tuple (cons <expr> <expr>)"
             "\n\tan application (<expr> <expr>)"
             "\n\ta variable, 0, unit, or set"
             "\n\nthe expression given was of type:\n\t~a")
           e*))]))

(: ext-pi (-> Val (Pairof Val Clos)))
(define (ext-pi t)
  (match t 
    [(vpi t* g)
     (cons t* g)]
    [_ (error (format "error in ext-pi with:\n\n\t~a"
                      t))]))

(: ext-sigma (-> Val (Pairof Val Clos)))
(define (ext-sigma t)
  (match t 
    [(vsigma t* g)
     (cons t* g)]
    [_ (error (format "error in ext-sigma with:\n\n\t~a"
                      t))]))

(: check-decl (-> Integer Telescope Gamma Decl Gamma))
(define (check-decl k rho gamma d)
  (when (unbox *debug-mode*)
  (displayln (format "check-decl with:\n\t~a\n\t~a\n\t~a\n\t~a"
                     k rho gamma d)))
  (match d 
    [(ddec p a e)
     (begin (check-type k rho gamma a)
            (define t (evaluate a rho))
            (check k rho gamma e t)
            (up-g gamma p t (evaluate e rho)))]
    [(drec p a e)
     (begin (check-type k rho gamma a)
            (define t (evaluate a rho))
            (define gen (gen-v k))
            (define gamma* (up-g gamma p t gen))
            (pretty-display gamma*)
            (check (add1 k) (tupvar rho p gen) gamma* e t)
            (define v (evaluate e (tupdec rho d)))
            (up-g gamma p t v))]))

(: equal-nf? (-> Integer Val Val Boolean))
(define (equal-nf? k m n)
  (when (unbox *debug-mode*)
    (displayln (format "equal-nf? with ~a ~a ~a"
                       k m n)))
  (begin (define e (read-back-val k m))
         (define g (read-back-val k n))
         (equal? e g)))

(: check-main (-> Boolean Expr Val Void))
(define (check-main debug e t)
  (set-box! *debug-mode* debug)
  (when (unbox *debug-mode*)
  (displayln (format "check-main with:\n\t~a\n\t~a" e t)))
  (check 0 (tnil) (hash) e t))

