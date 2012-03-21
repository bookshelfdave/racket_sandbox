#lang racket
(define testdata (open-input-string "This\nString to parse\n1000"))

(struct lexer (data pos line col lasttoken) #:transparent)
(struct token (type data line col) #:transparent)

(define testlex (lexer testdata 0 0 0 'notoken))

(define (show-lexer l)
  (display "Next Char:[")
  (display (peek-char (lexer-data l)))
  (display "] ")  
  (display "Pos:")
  (display (lexer-pos l))
  (display " Line:")
  (display (lexer-line l))
  (display " Col:")
  (display (lexer-col l))
  (newline))

(define (show-token t)
  (display "Token<")
  (display (token-type t))
  (display "> [")
  (display (token-data t))
  (display "] Line:")
  (display (token-line t))
  (newline))
 
(define match-range  
  (lambda (startchar finishchar)
    (lambda (c)       
       (if (and (char<=? startchar c) 
                (char>=? finishchar c))
           #t
           #f))))

(define match-lowercase (match-range #\a #\z))
(define match-uppercase (match-range #\A #\Z))
(define (match-letter c)
  (or (match-uppercase c) (match-lowercase c)))
(define match-number (match-range #\0 #\9))

                                                             
(define (match-char mchar)
  (lambda (c)
    (if (equal? mchar c)
        #t
        #f
        )))

  
(define (idmatchers c)
  (or (match-uppercase c) (match-lowercase c) (match-number c))) 

(define (consume l matcher tokentype)  
  (define (consumer seek)    
    (let* ([c (peek-char-or-special (lexer-data l) seek )])              
      (cond 
        [(eof-object? c) seek]
        [(matcher c) (consumer (+ 1 seek))]
        [else seek])))     
  (let* ([seeklen (consumer 1)]
         [d (read-string seeklen (lexer-data l)) ])    
    (struct-copy lexer l [pos seeklen] [lasttoken (token tokentype d 0 0)])))

(define (ignore-char l)
  (read-char (lexer-data l))
  (struct-copy lexer l 
               [pos (+ (lexer-pos l) 1)]))  

(define (consume-newline l)
  (read-char (lexer-data l))
  (struct-copy lexer l 
               [pos (+ (lexer-pos l) 1)]
               [line (+ (lexer-line l) 1)]
               [lasttoken 'newline]))  

(define (consume-space l)
  (read-char (lexer-data l))
  (struct-copy lexer l 
               [pos (+ (lexer-pos l) 1)]
               [lasttoken 'space]))  

(define (consume-tab l)
  (read-char (lexer-data l))
  (struct-copy lexer l 
               [pos (+ (lexer-pos l) 1)]
               [lasttoken 'tab]))  

(define (next-token l)  
  (let ([c (peek-char (lexer-data l))])
    (cond 
      [(eof-object? c) (display "Done!")]
      [(match-number c) (consume l match-number 'int)]
      [(match-uppercase c) (consume l idmatchers 'typeid)]
      [(match-lowercase c) (consume l idmatchers 'id)]
      [(match-char #\space) (consume-space l) ]
      [(match-char #\tab) (consume-tab)]
      [(match-char #\newline) (consume-newline l)]
      [else (display "Done")]))    
  )