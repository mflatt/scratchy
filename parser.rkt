#lang racket/base
(require parser-tools/lex 
         (prefix-in : parser-tools/lex-sre)
         algol60/cfg-parser
         syntax/readerr
         (for-syntax racket/base))

(provide lex
         parse
         token->string)

(define (parse src-name in)
  (parameterize ([current-source src-name])
    (parse-from-lex (lambda () (lex in)))))

;; ----------------------------------------
;; Lexer

(define-tokens content-tokens
  (ID NUM SPECIAL ERROR))

(define-empty-tokens delim-tokens
  (EOF SEP IS OPEN CLOSE ASSIGN 
       PLUS MINUS TIMES DIVIDE
       IMAGE MOVE X Y CHANGE SIZE
       TURN TO ON KEY SAY HUSH
       DO FOREVER FORWARD IF WHILE SLEEP
       RANDOM VARIABLE TOUCHES DIRECTION
       WHITESPACE))

(define lex
  (lexer-src-pos
   [(:seq "@" (:+ (:or #\- (:/ #\A #\Z #\a #\z)))) 
    (token-ID (string->symbol (substring lexeme 1)))]
   [(:seq (:? (:or "-" "+")) (:+ (:/ "0" "9")) (:? ".") (:* (:/ "0" "9")))
    (token-NUM (parameterize ([read-decimal-as-inexact #f])
                 (string->number lexeme)))]
   [(:seq "---" (:* "-")) 'SEP]
   ["is" 'IS]
   ["{" 'OPEN]
   ["}" 'CLOSE]
   ["=" 'ASSIGN]
   ["+" 'PLUS]
   ["-" 'MINUS]
   ["*" 'TIMES]
   ["/" 'DIVIDE]
   ["image" 'IMAGE]
   ["key" 'KEY]
   ["move" 'MOVE]
   ["x" 'X]
   ["y" 'Y]
   ["size" 'SIZE]
   ["direction" 'DIRECTION]
   ["change" 'CHANGE]
   ["turn" 'TURN]
   ["to" 'TO]
   ["on" 'ON]
   ["do" 'DO]
   ["forever" 'FOREVER]
   ["forward" 'FORWARD]
   ["if" 'IF]
   ["while" 'WHILE]
   ["random" 'RANDOM]
   ["variable" 'VARIABLE]
   ["sleep" 'SLEEP]
   ["say" 'SAY]
   ["hush" 'HUSH]
   ["touches" 'TOUCHES]
   [(:+ (:/ #\A #\Z #\a #\z)) (token-ID (string->symbol lexeme))]
   [(:+ whitespace) 'WHITESPACE]
   [(special) (token-SPECIAL lexeme)]
   [(eof) 'EOF]
   [any-char (token-ERROR lexeme)]))

(define parse-from-lex
  (cfg-parser
   (start <prog>)
   (end EOF)
   (tokens content-tokens
           delim-tokens)
   (precs)
   (error (lambda (a t v start end) 
            (raise-parse-error t v start end)))
   (src-pos)
   (grammar
    (<prog> [(<ws>) null]
            [(<ws> <sprite> <prog>) (cons $2 $3)])
    (<sprite> [(SEP <ws> ID <clauses>) 
               (at-src `(define-sprite ,$3 . ,$4))])
    (<clauses> [(<ws>) null]
               [(<ws> <clause> <clauses>) (append $2 $3)])
    (<clause> [(IMAGE <ws> IS <ws> <expr>) (list (at-src '#:image) $5)]
              [(X <ws> IS <ws> <expr>) (list (at-src '#:x) $5)]
              [(Y <ws> IS <ws> <expr>) (list (at-src '#:y) $5)]
              [(SIZE <ws> IS <ws> <expr>) (list (at-src '#:size) $5)]
              [(DIRECTION <ws> IS <ws> <expr>) (list (at-src '#:direction) $5)]
              [(ON <ws> <key> <ws> KEY <ws> <stmts>) (list* (at-src '#:key)
                                                            $3
                                                            $7)]
              [(VARIABLE <ws> ID <ws> IS <ws> <expr>) (list (at-src '#:variable)
                                                            $3
                                                            $7)]
              [(DO <ws> <stmts>) (cons (at-src '#:task) $3)])
    (<expr> [(NUM) $1]
            [(ID) $1]
            [(SPECIAL) (at-src `(quote ,$1))]
            [(RANDOM <ws> <expr>) (at-src `(random ,$3))] 
            [(TOUCHES <ws> <expr>) (at-src `(touches? ,$3))] 
            [(<expr> <ws> PLUS <ws> <expr>) (at-src `(+ ,$1 ,$5))] 
            [(<expr> <ws> MINUS <ws> <expr>) (at-src `(- ,$1 ,$5))]
            [(<expr> <ws> TIMES <ws> <expr>) (at-src `(* ,$1 ,$5))]
            [(<expr> <ws> DIVIDE <ws> <expr>) (at-src `(/ ,$1 ,$5))])
    (<stmt> [(MOVE <ws> X <ws> <expr>) (at-src `(move-x ,$5))]
            [(MOVE <ws> Y <ws> <expr>) (at-src `(move-y ,$5))]
            [(TURN <ws> TO <ws> <expr>) (at-src `(turn-to ,$5))]
            [(TURN <ws> <expr>) (at-src `(turn ,$3))]
            [(FORWARD <ws> <expr>) (at-src `(forward ,$3))]
            [(CHANGE <ws> SIZE <ws> <expr>) (at-src `(change-size ,$5))]
            [(SLEEP <ws> <expr>) (at-src `(sleep ,$3))]
            [(FOREVER <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(forever . ,$5))]
            [(IF <ws> <expr> <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(when ,$3 . ,$7))]
            [(WHILE <ws> <expr> <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(while ,$3 . ,$7))]
            [(ID <ws> ASSIGN <ws> <expr>) (at-src `(set! ,$1 ,$5))]
            [(SAY <ws> <expr>) (at-src `(say ,$3))]
            [(HUSH) (at-src '(hush))])
    (<stmts> [() '()]
             [(<ws> <stmt> <stmts>) (cons $2 $3)])
    (<key> [(ID) $1]
           [(PLUS) (at-src '+)]
           [(MINUS) (at-src '-)]
           [(TIMES) (at-src '*)]
           [(DIVIDE) (at-src '/)])
    (<ws> [() #f]
          [(WHITESPACE) #f]))))

(define-syntax (at-src stx)
  (syntax-case stx ()
    [(_ e) 
     (with-syntax ([start (datum->syntax stx '$1-start-pos)]
                   [end (datum->syntax stx '$n-end-pos)])
       #'(datum->syntax #f e (to-srcloc start end) orig-prop))]))

(define orig-prop (read-syntax 'src (open-input-bytes #"x")))

;; ----------------------------------------
;; Source locations and error reporting:

(define current-source (make-parameter #f))

(define (to-srcloc start end)
  (list
   (current-source)
   (position-line start)
   (position-col start)
   (position-offset start)
   (and (position-offset end)
        (position-offset start)
        (- (position-offset end)
           (position-offset start)))))

(define (raise-parse-error t v start end)
  (apply
   (if (eq? t 'EOF) raise-read-eof-error raise-read-error) 
   (format "bad syntax at ~a" (token->string t v))
   (to-srcloc start end)))

(define (token->string t v)
  (if v
      (format "~a" v)
      (format "~a" t)))
