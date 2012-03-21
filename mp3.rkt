#lang racket
(require rackunit)
(require rackunit/gui)


; structures
(provide lexer token parser)
(provide next-token next-raw-token parse)

(define testcode "module Foo")
(define testdata "var Int foo = 100")

(struct lexer (data pos line col lasttoken))
(struct token (type value line col) #:transparent)
(struct parser (col line lex))

(define testlexer (lexer testdata 0 0 0 'notoken))
(define testparser (parser 0 0 testlexer))
  
(define (show-lexer l)
  (display "Next Char:[")
  (display (if 
            (symbol? (lexer-data l)) 
            (symbol->string (lexer-data l)) 
            (lexer-data l)))
  (display "] ")  
  (display "Pos:")
  (display (lexer-pos l))
  (display " Line:")
  (display (lexer-line l))
  (display " Col:")
  (display (lexer-col l))
  (display " Last Token:")
  (display (lexer-lasttoken l))
  (newline))

(define (show-token t)
  (if (equal? t 'notoken)
      (display "")
      (begin 
        (display "Token<")
        (display (token-type t))
        (display "> [")
        (display (token-value t))
        (display "] Line:")
        (display (token-line t))
        (display " Col:")
        (display (token-col t))
        (newline))))
 
; a matcher will return a raw string to next-token
; next-token will check for newlines and set row and col info
; it will then build a token and set the lasttoken value of the lexer

(define (make-matcher re ty)
  (lambda (line) 
    (let ([result (regexp-match re line)])
      (cond
        [(equal? result #f) (list 'invalid 'invalid)]
        [else (list (car result) ty)]))))

(define reserved (list "def" "package" "true" "false" "var" "val" "new"))

(define eqeq-matcher   
  (make-matcher #rx"^==" 'eqeq))

(define gteq-matcher   
  (make-matcher #rx"^>="  'gteq))

(define lteq-matcher   
  (make-matcher #rx"^<="  'lteq))   

(define plus-matcher   
  (make-matcher #rx"^\\+"   'plus))

(define minus-matcher   
  (make-matcher #rx"^-"   'minus))

(define divide-matcher   
  (make-matcher #rx"^/"   'divide))

(define mult-matcher   
  (make-matcher #rx"^\\*"   'mult))

(define power-matcher   
  (make-matcher #rx"^\\^"   'power))  

(define eq-matcher   
  (make-matcher #rx"^="   'eq))

(define lt-matcher   
  (make-matcher #rx"^<"   'lt))

(define gt-matcher   
  (make-matcher #rx"^>"   'gt))

(define bang-matcher   
  (make-matcher #rx"^!"   'bang))

(define lparen-matcher   
  (make-matcher #rx"^\\("   'lparen))

(define rparen-matcher   
  (make-matcher #rx"^\\)"   'rparen))

(define lbracket-matcher   
  (make-matcher #rx"^\\["   'lbracket))

(define rbracket-matcher   
  (make-matcher #rx"^\\]"   'rbracket))

(define colon-matcher   
  (make-matcher #rx"^:"   'colon))


(define newline-matcher
  (make-matcher #rx"^[\n]+" 'newline))

(define whitespace-matcher 
  (make-matcher #rx"^[\t\\ ]+" 'whitespace))

;(define tab-matcher 
;  (make-matcher #rx"^[\t]+" 'tab))

(define type-matcher 
  (make-matcher #rx"^[A-Z][A-Za-z0-9_]*" 'type))

(define id-matcher   
  (make-matcher #rx"^[a-z][A-Za-z0-9_]*" 'id))

(define num-matcher   
  (make-matcher #rx"^[0-9]+" 'number))


; ] must be the first char
; - must be the last char
;(define ops-matcher  
;  (make-matcher #rx"^[]+/*^=><!()[-]+" 'op ))

(define matchers 
  (list num-matcher 
        id-matcher 
        type-matcher 
        whitespace-matcher 
        newline-matcher         
        eqeq-matcher   
        gteq-matcher   
        lteq-matcher   
        plus-matcher   
        minus-matcher   
        divide-matcher   
        mult-matcher   
        power-matcher   
        eq-matcher   
        lt-matcher   
        gt-matcher   
        bang-matcher   
        lparen-matcher   
        rparen-matcher   
        lbracket-matcher   
        rbracket-matcher   
        colon-matcher
        ))
  
; probably very slow :-)
(define (trymatch ms line)
  (if (= (string-length line) 0)
      'eof      
      (let ( [result (filter (lambda (x) (string? (car x))) (map (lambda (m) (m line)) ms))])    
        (cond
          [(not (empty? result)) (car result)]
          [else 'nomatch]))))
    
(define (getcolumn l txt)
  (if (> (length (regexp-match* #rx"\n" txt)) 0)
      ;; not correct, but probably fine for now.
      (string-length (substring txt (cdr (last (regexp-match-positions* #rx"\n" txt)))))
      (+ (lexer-col l) (string-length txt))))

(define (checkreserved value tt)
  (if (equal? tt 'id) 
       (if (member value reserved)
           (string->symbol (car (member value reserved)))
           tt)
      tt))


(define (consume l match)
  (let* (
         [tokentxt  (car match)]
         [tokentype (car (cdr match))]
         [tlen (string-length tokentxt)]
         [newlinecount (length (regexp-match* #rx"\n" tokentxt))]
         [newcol (getcolumn l tokentxt)]
         [newpos (+ (lexer-pos l) tlen)]
         [newdata (substring (lexer-data l) tlen)]
         [newlinenum (+ (lexer-line l) newlinecount)]
         [newtt (checkreserved tokentxt tokentype)]
         [newtoken (token newtt tokentxt (lexer-line l) (lexer-col l))]
         )        
    (struct-copy lexer l 
                 [col newcol]
                 [pos newpos]
                 [lasttoken newtoken]
                 [data newdata]
                 [line newlinenum])))



(define (next-raw-token l)          
  (let ([match (trymatch matchers (lexer-data l))])
    (cond 
      [(equal? match 'eof) (struct-copy lexer l [lasttoken 'eof])]
      [(equal? match 'nomatch) 
       (struct-copy lexer l 
                    [lasttoken (token 'invalid 'invalid (lexer-line l) (lexer-col l))])]
      [else (consume l match)]      
      )))



(define (iswhitespace? l)
  (if (and (token? (lexer-lasttoken l))
           (equal? (token-type (lexer-lasttoken l)) 'whitespace))
      #t
      #f))

(define (isnewline? l)
  (if (and (token? (lexer-lasttoken l))
           (equal? (token-type (lexer-lasttoken l)) 'newline))
      #t
      #f))


(define (isindent? currlex nextlex)
  (if (and (equal? (lexer-col currlex) 0)
           (iswhitespace? nextlex))
      #t
      #f))


; skips whitespace
; builds indentations from raw tokens
(define (next-token currlex)
  (let* ([nextlex (next-raw-token currlex)]
         [nexttoken (lexer-lasttoken nextlex)])
    
    (cond
      [(isindent? currlex nextlex) 
       (struct-copy lexer nextlex 
                    [lasttoken
                     (token 'indent 
                            (string-length (token-value nexttoken)) 
                            (lexer-line currlex)
                            (lexer-col currlex))])]
      [(iswhitespace? nextlex) (next-token nextlex)]
      [(isnewline? nextlex) (next-token nextlex)]
      [else nextlex])))

(define (testit l) 
  (show-token (lexer-lasttoken l))
  (let ([nl (next-token l)])        
    (cond
      [(equal? (lexer-lasttoken nl) 'eof) (display "Done!")]
      [(equal? (token-type (lexer-lasttoken nl)) 'invalid) 
       (display "Parse error at line ")       
       (display (token-line (lexer-lasttoken nl)))
       (display " column ")
       (display (token-col (lexer-lasttoken nl)))
       (newline)
       ] 
      [else
       (begin          
          (testit nl))])))

(define (parseerror nl)
  (display "Syntax error at line ")       
  (display (token-line (lexer-lasttoken nl)))
  (display " column ")
  (display (token-col (lexer-lasttoken nl)))
  (newline))
  
(define (parse p) 
  (show-token (lexer-lasttoken (parser-lex p)))  
  (let* ([nl (next-token (parser-lex p))]
         [curr (lexer-lasttoken nl)])        
    (cond
      [(equal? curr 'eof) 
       (display "Done!")]
      [(equal? (token-type curr) 'invalid) 
       (parseerror nl)] 
      [else
       (begin          
          (parse (struct-copy parser p [lex nl])))])))


(define (matchtoken tt)
  (lambda (lex)
    (if (and (token? (lexer-lasttoken lex))
             (equal? (token-type (lexer-lasttoken lex)) tt))
        #t
        #f)))


; Start with these matchers:
; *, +, ?

(define (testmatchers lex matchers)
  (define (testmatchers0 l matchers tokens)
    (cond
      [(empty? matchers) (list l (reverse tokens))]
      [(list? (car matchers)) (display "Not implemented")]
      [else (let* ([matcher (matchtoken (car matchers))]
                   [nl (next-token l)]
                   [token (lexer-lasttoken nl)])
              (if (matcher nl)
                  (testmatchers0 nl (cdr matchers) (cons token tokens))
                  (list lex '())))]))
  (testmatchers0 lex matchers '()))

           
;(define-test-suite CompilerTests 
;  (check-equal? (type-matcher "Foo") '("Foo" type) "Type matching")
;  (check-equal? (id-matcher "foo") '("foo" id) "ID matching")
; )
;(test/gui CompilerTests)