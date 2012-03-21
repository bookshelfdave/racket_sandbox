#lang racket
(require racket/file)
(define filedata (file->string "c:\\src\\foo.txt"))

(struct lexer (data pos line col))


 
(struct token (type data line col))

(define (makelx input)
  (lexer (string->list input) 0 0 0))


(define (peeknext lx)
  (if (not (empty? (lexer-data lx)))      
      (car (lexer-data lx))
      '()))

(define (lexconsume l) (struct-copy lexer l [data (cdr (lexer-data l))]))

; test stuff
(define p (makelx "fooBar"))
(define p1 (struct-copy lexer p [data (cdr (lexer-data p))]))
(define samplelx (makelx filedata))

;; character matchers
(define matchrange  
  (lambda (startchar finishchar)
    (lambda (c)       
       (if (and (char<=? startchar c) 
                (char>=? finishchar c))
           #t
           #f))))
(define match-lowercase-letter (matchrange #\a #\z))
(define match-uppercase-letter (matchrange #\A #\Z))
(define (match-letter c)
  (or (match-uppercase-letter c) (match-lowercase-letter c)))
(define match-number (matchrange #\0 #\9))


(define (match-or a b)
  (lambda (c)
    (or (a c) (b c))))

;(define (matchoneof matchers)
;  (lambda (c)     


;; each matcher should return a tuple that contains matched length and the possibly new lexer




; need to return a lambda here
(define oneof
  (lambda (c matchers)
    (if (null? matchers)
        #f
        (if (equal? ((car matchers) c) #t)
            #t
            (oneof c (cdr matchers))))))


;(define noneof
;  (lambda (matchers)
;    (lambda (c)
;      (if (null? matchers)
;          #t
;          (if (equal? ((car matchers) c) #f)
;              (noneof c (cdr matchers))
;              #t
;              )))))



;(define (takewhile lx matcher)
;  (define takewhile2
;    (lambda (lx matcher len)    
;      (if (matcher (car (lexer-data lx)))
;          (takewhile2 (struct-copy lexer lx [data (cdr (lexer-data lx))]) matcher (+ 1 len))
;          (print len)
;          )))
;  (takewhile2 lx matcher 0)
;  )


(define (one-or-more lx matcher tokentype)
  (define (one-or-more0 lx matcher len)
    (cond 
      [(empty? (peeknext lx)) len]
      [(matcher (peeknext lx)) (one-or-more0 (lexconsume lx) matcher (+ 1 len))]
      [else len]))
  (let ([matchlength (one-or-more0 lx matcher 0)])
    (if (> matchlength 0)         
        (token tokentype (list->string (take (lexer-data lx) matchlength)) 0 0)
        '())
    ))

(define zero-or-more 1)

(define zero-or-one 1)


(define match-exactly 1)

(define lex   
  (lambda (lx)
    (let* ([pos (lexer-pos lx)]
           [data (lexer-data lx)]
           [c (string-ref (lexer-data lx) pos)])      
      (print c)      
      (if (< pos (- (string-length data) 1))
          (lex (struct-copy lexer lx [pos (+ pos 1)]))
          (display "Done")))))

(define lex2
  (lambda (lx)
    (let* ([c (car (lexer-data lx))]
           [data (lexer-data lx)])
      (print c)
      (if (not (empty? (cdr data)))
          (lex2 (struct-copy lexer lx [data (cdr data)]))
          (display "Done")))))
           

(define fail (vector 'fail)) ; fail is not eq? to anything else 
(define (unit x) x) 
(define (bind m f) 
  (if (eq? m fail) 
      fail 
      (f m))) 

 
(define foo (lambda (x) (+ x 1)))
(define bar (lambda (x) (+ x 2)))
(define baz (lambda (x) (+ x 4)))


(define (compose* f x) 
        (if (eq? x #f) 
            #f 
            (f x))) 

(define (foo-bar-baz x) (compose* baz (compose* bar (foo x))))

