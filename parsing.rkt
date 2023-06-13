#lang racket

(provide parse)

(require parser-tools/yacc
         "lexing.rkt"
         "expr.rkt")

(define rktttp
  (parser
   [start decls]
   [end EOF]
   [error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (begin
        (printf
         "tok-ok? = ~a\ntok-name = ~a\ntok-value = ~a\nstart-pos = ~a\nend-pos = ~a\n"
         tok-ok?
         tok-name
         tok-value
         start-pos
         end-pos)
        (void)))]
   [tokens empty-tokens literal-tokens]
   [src-pos]
   [yacc-output "debug/debug.y"]
   [debug "debug/debug.lalr"]
   [grammar
     [decls 
       [() empty]
       [(decl) (list $1)]
       [(decl decls) (cons $1 $2)]]
     [decl 
       [(LPAREN LET tvar expr RPAREN)
        (ddec (car $3) (cdr $3) $4)]
       [(LPAREN LETREC tvar expr RPAREN)
        (drec (car $3) (cdr $3) $4)]]
     [tvar
       [(LPAREN COLON patt expr RPAREN)
        (cons $3 $4)]]
     [patt 
       [(WILD)
        (pwild)]
       [(IDENT)
        (pvar $1)]
       [(LPAREN COMMA patt patt RPAREN)
        (ppair $3 $4)]]
     [expr
       [(IDENT)
        (evar $1)]
       [(LABEL)
        (econ $1 (eone))]
       [(LPAREN LABEL expr RPAREN)
        (econ $2 $3)]
       [(LPAREN expr expr RPAREN)
        (eapp $2 $3)]
       [(LPAREN CONS expr expr RPAREN)
        (etuple $3 $4)]
       [(LPAREN LAMBDA patt expr RPAREN)
        (elam $3 $4)]
       [(LPAREN PI tvar expr RPAREN)
        (epi (car $3) (cdr $3) $4)]
       [(LPAREN SIGMA tvar expr RPAREN)
        (esigma (car $3) (cdr $3) $4)]
       [(LPAREN ARROW expr expr RPAREN)
        (epi (pwild) $3 $4)]
       [(LPAREN STAR expr expr RPAREN)
        (esigma (pwild) $3 $4)]
       [(SET)
        (eset)]
       [(UNIT)
        (eunit)]
       [(ONE)
        (eone)]
       [(LPAREN LABEL expr RPAREN)
        (econ $2 $3)]
       [(LPAREN SUM LBRACK branches RBRACK RPAREN)
        (esum (make-immutable-hash $4))]
       [(LPAREN FUN LBRACK branches RBRACK RPAREN)
        (efun (make-immutable-hash $4))]
       [(LPAREN CAR expr RPAREN)
        (efst $3)]
       [(LPAREN CDR expr RPAREN)
        (esnd $3)]
       [(VOID)
        (evoid)]]
     [branch 
       [(LABEL)
        (cons $1 (eunit))]
       [(LPAREN LABEL expr RPAREN)
        (cons $2 $3)]
       [(LPAREN FATARROW LABEL expr RPAREN)
        (cons $3 (elam (pwild) $4))]
       [(LPAREN FATARROW LPAREN LABEL patt RPAREN expr RPAREN)
        (cons $4 (elam $5 $7))]]
     [branches 
       [() empty]
       [(branch) (list $1)]
       [(branch branches) (cons $1 $2)]]]))

(define (parse inp)
  (rktttp (lambda () (get-token inp))))
