%token IDENT
%token LABEL
%token ERROR
%token TOKEN-ERROR
%start (decls)
%%
decls: 
| decl 
| decl decls 
;
decl: 'LPAREN' 'LET' tvar expr 'RPAREN' 
| 'LPAREN' 'LETREC' tvar expr 'RPAREN' 
;
tvar: 'LPAREN' 'COLON' patt expr 'RPAREN' 
;
patt: 'WILD' 
| IDENT 
| 'LPAREN' 'COMMA' patt patt 'RPAREN' 
;
expr: IDENT 
| LABEL 
| 'LPAREN' LABEL expr 'RPAREN' 
| 'LPAREN' expr expr 'RPAREN' 
| 'LPAREN' 'CONS' expr expr 'RPAREN' 
| 'LPAREN' 'LAMBDA' patt expr 'RPAREN' 
| 'LPAREN' 'PI' tvar expr 'RPAREN' 
| 'LPAREN' 'SIGMA' tvar expr 'RPAREN' 
| 'LPAREN' 'ARROW' expr expr 'RPAREN' 
| 'LPAREN' 'STAR' expr expr 'RPAREN' 
| 'SET' 
| 'UNIT' 
| 'ONE' 
| 'LPAREN' LABEL expr 'RPAREN' 
| 'LPAREN' 'SUM' 'LBRACK' branches 'RBRACK' 'RPAREN' 
| 'LPAREN' 'FUN' 'LBRACK' branches 'RBRACK' 'RPAREN' 
| 'LPAREN' 'CAR' expr 'RPAREN' 
| 'LPAREN' 'CDR' expr 'RPAREN' 
| 'VOID' 
;
branch: LABEL 
| 'LPAREN' LABEL expr 'RPAREN' 
| 'LPAREN' 'FATARROW' LABEL expr 'RPAREN' 
| 'LPAREN' 'FATARROW' 'LPAREN' LABEL patt 'RPAREN' expr 'RPAREN' 
;
branches: 
| branch 
| branch branches 
;
%%
