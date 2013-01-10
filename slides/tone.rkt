#lang racket
(require (for-syntax syntax/parse
                     racket/format))

(begin-for-syntax
 (struct tone (freq)
   #:property 
   prop:procedure 
   (lambda (self stx)
     (syntax-parse stx
       [_:id (tone-freq self)])))
 (struct duration (len)))
 
(define-syntax A (tone #'440))
(define-syntax ♩ (duration #'1/4))

(begin-for-syntax
 (define (check id pred what stx)
   (unless (pred (syntax-local-value id (lambda () #f)))
     (raise-syntax-error #f (~a "not a " what) id stx))))

(define-syntax (note stx)
  (syntax-parse stx
    [(_ tn:id dr:id)
     (check #'tn tone? "tone" stx)
     (check #'dr duration? "duration" stx)
     #`(list #,(tone-freq (syntax-local-value #'tn))
             #,(duration-len (syntax-local-value #'dr)))]))

(note A ♩)
(A)

