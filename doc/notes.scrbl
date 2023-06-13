#lang scribble/manual

@title{Bidirectional Typechecking of Dependent Types}

This algorithm is based of the language specification by Thierry
Coquand of the @italic{Mini-TT} language.

@section{The main function}

@codeblock{
(: check-main (-> Expr Val Void))
(define (check-main e t)
  (check 0 (tnil) (hash) e t))
}

This highlights the important different types of values already 
present within the algorithm.

First, @code{Expr}, which can be described by the following declaration:

@codeblock{
(define-type Expr 
  (U elam eset epi esigma eone eunit 
     econ esum efun efst esnd eapp 
     evar evoid edecl etuple))
}

Next, @code{Val}, which can be described by the following type declaration:

@codeblock{
(define-type Val (U vlam vpair vcon vunit vset 
                    vone vpi vsigma vfun vsum vnt))
}

The main difference between @code{Expr} and @code{Val} is that the latter represent
``an open expression in weak head normal form", and, 
``it is either a @italic{neutral value}
@math{[k]} which represents an expression whose computation stopped because of an attempt
to compute a variable, or a @italic{canonical value}, the form which makes clear the head construction 
of an expression, meaning that it is a lambda abstraction, a pi abstraction etc."

Note further that @code{check-main} is just a call to @code{check}.

@section{The function @code{check}}

The function @code{check} is defined as 

@codeblock{
(: check (-> Integer Telescope Gamma Expr Val Void))
(define (check k rho gamma e* t*)
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
}

Here we match @code{e*} and @code{t*}, which are the @italic{expression} which is being 
typechecked, and the @italic{type} which we are comparing the expression to.

As @code{check} is defined by cases, so we shall proceed through each case.

@subsection{Lambda Abstraction}

@codeblock{
    [((elam p e) (vpi t g))
     (define gen (gen-v k))
     (define gamma* (up-g gamma p t gen))
     (check (add1 k) (tupvar rho p gen) gamma* e (inst g gen))]
}

In typechecking a lambda abstraction, we generate a fresh variable @code{gen},
and then update @code{gamma}, our type environment, with the new variable.

We then declare that the domain of the abstraction @italic{is just that fresh variable}
to be checked, and then instantiate a new value based off of the codomain of the Pi type abstraction.

@subsection{Tuples}

@codeblock{
    [((etuple e1 e2) (vsigma t g))
     (check k rho gamma e1 t)
     (check k rho gamma e2 (inst g (evaluate e1 rho)))]
}

@subsection{Constructors}

@codeblock{
    [((econ c e) (vsum (sclos cas rho*)))
     (define a : (Option Expr) 
       (hash-ref cas c (lambda () #f)))
     (when a 
       (check k rho gamma e (evaluate a rho*)))]
}

@subsection{Pattern matching}

@codeblock{
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
}

@subsection{Unit and One}

@codeblock{
    [((eunit) (vset))
     (void)]
    [((eone) (vunit))
     (void)]
}

@subsection{Pi Abstraction}

@codeblock{
    [((epi p a b) (vset))
     (check k rho gamma a (vset))
     (define gen (gen-v k))
     (define gamma* (up-g gamma p (evaluate a rho) gen))
     (check (add1 k) (tupvar rho p gen) gamma* b (vset))]
}

@subsection{Sigma Abstraction}

@codeblock{
    [((esigma p a b) (vset))
     (check k rho gamma (epi p a b) (vset))]
}

@subsection{Sums}

@codeblock{
    [((esum cas) (vset))
     (for ([a (hash-values cas)])
       (check k rho gamma a (vset)))]
}

@subsection{Declarations}

@codeblock{
    [((edecl d e) t)
     (define gamma* (check-decl k rho gamma d))
     (check k (tupdec rho d) gamma* e t)]
}

@subsection{Otherwise...}

@codeblock{
    [(_ _)
      (define t** (check-infer k rho gamma e*))
      (when (equal-nf? k t* t**)
        (void))]
}
