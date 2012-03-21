#lang racket

(define (any-char s)
  (cond
    [(equal? (string-length s) 0) #f]
    [else (list (string-ref s 0) (substring s 1))]))

(define (chartest pred)
  (lambda (s)
    (let ([c (any-char s)])
      (cond 
        [(equal? c #f) #f]
        [(pred (string-ref s 0)) 
         (list (string-ref s 0) (substring s 1))]
        [else #f]))))

(define (matchchar c)
  (chartest (lambda (x) (equal? x c))))
      
(define (one-of s)
  (chartest (lambda (x) (member x (string->list s)))))
        

(define alpha
  (one-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define lower
  (one-of "abcdefghijklmnopqrstuvwxyz"))

(define upper
  (one-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(define digit
  (one-of "0123456789"))

(define whitespace
  (one-of " \t\n\r"))


(define (matchstr0 w s)      
  (if (equal? (string-length w) 0)
      (list "" s)      
      (if ((matchchar (string-ref w 0)) (string (string-ref s 0)))          
          (list w (cadr (matchstr0 (substring w 1) (substring s 1))))
            #f)))


(define (matchstr w)
  ((curry matchstr0) w))
    
(define (optional parsefn)
  (lambda (s)
    (let ([c (parsefn s)])
      (if (equal? #f c)
          (list (string->list "") s)
          c
      ))))

(define matchwhile (matchstr "while"))

(define (repeat+0 parsefn s)  
  (let ([result (parsefn s)])
    (if (equal? result #f)  
        #f        
        (let ([c (car result)]
              [cs (second result)])
          (string-append 
           (string (list c)) 
           cs)
          (repeat+0 parsefn (substring s 1)))))))
                      
;(define (repeat* parsefn) optional (repeat+ parsefn))