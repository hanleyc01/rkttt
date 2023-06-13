#lang racket

(provide (all-defined-out))

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; token definitions
(define-empty-tokens
  empty-tokens
  (EOF COMMENT WHITESPACE LPAREN RPAREN LBRACK RBRACK LAMBDA SET 
   DOT SIGMA PI ONE UNIT FUN SUM VOID LET LETREC WILD COLON COMMA
   CONS CAR CDR ARROW FATARROW STAR))

(define-tokens 
  literal-tokens 
  (IDENT LABEL ERROR TOKEN-ERROR))


(define rktttl
  (lexer-src-pos [(eof) (token-EOF)]
                 [(:or #\tab #\space #\return #\newline) (token-WHITESPACE)]
                 [(:: ";" (:* (char-complement #\newline))) (token-COMMENT)]
                 [(:: "#|" (:* (complement "|#") "|#")) (token-COMMENT)]
                 ["(" (token-LPAREN)]
                 [")" (token-RPAREN)]
                 ["[" (token-LBRACK)]
                 ["]" (token-RBRACK)]
                 ["lambda" (token-LAMBDA)]
                 ["set" (token-SET)]
                 ["sigma" (token-SIGMA)]
                 ["pi" (token-PI)]
                 ["unit" (token-UNIT)]
                 ["0" (token-ONE)]
                 ["fun" (token-FUN)]
                 ["sum" (token-SUM)]
                 ["void" (token-VOID)]
                 ["let" (token-LET)]
                 ["letrec" (token-LETREC)]
                 ["cons" (token-CONS)]
                 ["car" (token-CAR)]
                 ["cdr" (token-CDR)]
                 ["*" (token-STAR)]
                 ["->" (token-ARROW)]
                 ["=>" (token-FATARROW)]
                 ["," (token-COMMA)]
                 [":" (token-COLON)]
                 ["_" (token-WILD)]
                 ["." (token-DOT)]
                 [(:: (:or (:/ "a" "z") (:/ #\A #\Z) #\_ #\- #\*)
                      (:* (:or (:/ "a" "z") (:/ #\A #\Z) #\_) #\- #\* (:/ "0" "9")))
                  (token-IDENT lexeme)]
                 [(:: "'" (:or (:/ "a" "z") (:/ #\A #\Z) #\_ #\- #\*)
                      (:* (:or (:/ "a" "z") (:/ #\A #\Z) #\_) #\- #\* (:/ "0" "9")))
                  (token-LABEL (string->symbol lexeme))]))


(define (lex lexbuf)
  (begin
    (define (accum acc)
      (let ([res (rktttl lexbuf)])
        (if (equal? (position-token-token res) 'EOF)
            (reverse (cons res acc))
            (accum (cons res acc)))))
    (accum empty)))

(define (get-token in)
  (let ([res (rktttl in)])
    (if (or (equal? 'COMMENT (position-token-token res))
            (equal? 'WHITESPACE (position-token-token res)))
        (get-token in)
        res)))
