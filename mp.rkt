#lang racket

(require racket/file)
(define filedata (file->string "c:\\src\\foo.txt"))

(struct lexer (data pos line col))
(struct token (type data line col))
(struct lexresult (lex tok matchlen)) 
                   
(define (showlexer l)
  (display "Data:[")
  (display (list->string (lexer-data l)))
  (display "]")
  (newline)
  (display "Pos:")
  (display (lexer-pos l))
  (display " Line:")
  (display (lexer-line l))
  (display " Col:")
  (display (lexer-col l))
  (newline))

(define (showtoken t)
  (display "Token<")
  (display (token-type t))
  (display "> [")
  (display (list->string (token-data t)))
  (display "] Line:")
  (display (token-line t))
  (newline))
 
(define (showresult r)
  (showlexer (lexresult-lex r))
  (showtoken (lexresult-tok r))
  (display "Match length")
  (display (lexresult-matchlen r)))

(define (makelx input)
  (lexer (string->list input) 0 0 0))


(define (peeknext lx)
  (if (not (empty? (lexer-data lx)))      
      (car (lexer-data lx))
      '()))

;; character matchers
(define match-range  
  (lambda (startchar finishchar)
    (lambda (c)       
       (if (and (char<=? startchar c) 
                (char>=? finishchar c))
           1
           0))))

(define match-lowercase-letter (match-range #\a #\z))
(define match-uppercase-letter (match-range #\A #\Z))
(define (match-letter c)
  (or (match-uppercase-letter c) (match-lowercase-letter c)))
(define match-number (match-range #\0 #\9))

                                                             
(define (match-char mchar)
  (lambda (c)
    (if (equal? mchar c)
        1
        0
        )))
; need to return a lambda here
; NOT CORRECT
(define oneof
  (lambda (c matchers)
    (if (null? matchers)
        0
        (if (>  ((car matchers) c) 0)
            1
            (oneof c (cdr matchers))))))

; [A-Z][a-z1-9]*
;(define (<*> matchers)
;  (lambda (matchers len matchanyways)
;    (if (empty? matchers)
;        (list len matchanyways)
;        ((car matchers)
        

(define (<?>)
  1)

(define (<+>)
  1)

(define (<n> matchers)
  1)

(define (lexconsume l n tokenname)
  (let* ([consumed (take (lexer-data l) n)]
         [newlinecount (count (lambda (c) (equal? c #\newline)) consumed)])    
    (list (struct-copy lexer l 
                 [data (drop (lexer-data l) n)] 
                 [line (+ (lexer-line l) newlinecount)]
                 [pos  (+ (lexer-pos l) (length consumed))]
                 ) (token tokenname consumed (+ (lexer-line l) newlinecount) 0))))

;(define (lex-type lex tokenname)
; (if (empty? (lexer-data lex))      
;      (lexresult lex 'eof 0)

(define (lex-word word lex tokenname)
  (if (empty? (lexer-data lex))      
      (lexresult lex 'eof 0)
      ((lambda (word lex matchlen)
         (let* ([chars (string->list word)]
                [wordlen (length chars)])
           (if (< (length chars) wordlen)                       
               (lexresult lex 'notoken 0)
               (if (equal? (take (lexer-data lex) wordlen) chars)   
                   (let ([lc (lexconsume lex wordlen tokenname)])
                     (lexresult (first lc) (second lc) wordlen))            
                   (lexresult lex 'notoken 0))))) word lex 0)))
  


; test stuff
(define p (makelx "foobar 123 ! <test>\ntest"))
(define q (makelx "\n\nfoo"))
(define p1 (struct-copy lexer p [data (cdr (lexer-data p))]))
(define samplelx (makelx filedata))

;(define (lex-matchers matchers lex)
;  (let ([lm (lambda (matchers lex matchlen)
;              (let ([foo 1])
;                (if (empty? matchers)
;                    lex                   
;                    (let ([])
;                     
;                    ))))])
;    (lm matchers lex 0)
;    ))
   
 

    
  