#lang nanopass

(define variable?
  (lambda (x)
    (string? x)))

(define label?
  (lambda (l)
    (symbol? l)))

(define eprim?
  (lambda (e)
    (memq e '(0 set void unit))))

(define wild?
  (lambda (w)
    (equal? w '_)))

(define (arrow? a)
  (equal? a '=>))

(define (small-arrow? a)
  (equal? a '->))

(define-language L0
 [entry Decls]
 [terminals 
   (variable (x))
   (label (l))
   (eprim (pr))
   (arrow (a))
   (small-arrow (s))
   (wild (w))]
 [Patt (p)
  x
  w
  (values p1 p)]
 [Tvar (tvar)
  (: p e)]
 [Branch (b)
  l
  (l e)
  (a (l p) e)
  (a l e)]
 [Expr (e)
  (e1 e2)
  (lambda p e)
  x 
  pr
  'l 
  (* e1 e2)
  (s e1 e2)
  (pi tvar e2)
  (sigma tvar e2)
  (fun [b* ... b])
  (sum [b* ... b])
  w]
 [Decl (d)
  (let tvar e*)
  (letrec tvar e*)]
 [Decls (ds)
  (d ds)
  d])

(define-parser pl0 L0)

#|
(pl0 `((let (: ,"fart" set) set)
       (let (: ,"foo" unit) 0)))

(pl0 `(let (: ,"fart" set)
        (sum [,'foo 
              (=> (,'fart _) set)
              (=> ,'fart set)])))
|#

(define-language L1 
 (extends L0)
 (Expr (e) 
  (- (s e1 e2)
     (* e1 e2))))

(define-pass pass0 : L0 (ir) -> L1 ()
  (Expr : Expr (ir) -> Expr ()
    [(,s ,e1 ,e2)
      `(pi (: _ ,e1) ,e2)]
    [(* ,e1 ,e2)
      `(sigma (: _ ,e1) ,e2)])
  (Expr ir)
  (Decl : Decl (ir) -> Decl ()
   [(,let ,p ,e)
    `(let ,p ,e)]
   [(,letrec ,p ,e)
    `(letrec ,p ,e)]))

(define testin
  '(let (: "f" (-> set set))
                (lambda "a"
                  set)))

(define (test p)
  (begin
    (define parse-res (pl0 p))
    (pretty-display parse-res)
    (define pass-res (pass0 parse-res))
    (pretty-display pass-res)))

(test testin)
