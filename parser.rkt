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
  (ID NUM BINOP BINKEYOP UNBINKEYOP
      STRING SPECIAL ERROR))

(define-empty-tokens delim-tokens
  (EOF SEP IS OPEN CLOSE POPEN PCLOSE ASSIGN 
       PLUS MINUS TIMES DIVIDE
       IMAGE MOVE X Y CHANGE SIZE
       TURN TO BY ON KEY MESSAGE SEND EVERYONE SAY HUSH
       HIDE SHOW
       DO FOREVER FORWARD IF WHILE WAIT
       WATCH
       RANDOM VARIABLE TOUCHES DIRECTION
       USE WHITESPACE))

(define lex
  (lexer-src-pos
   [(:seq "@" (:+ (:or #\- #\_ #\? (:/ #\A #\Z #\a #\z #\0 #\9)))) 
    (token-ID (string->symbol (substring lexeme 1)))]
   [(:or (:seq (:+ (:/ "0" "9")) (:? ".") (:* (:/ "0" "9")))
         (:seq "." (:* (:/ "0" "9"))))
    (token-NUM (parameterize ([read-decimal-as-inexact #f])
                 (string->number lexeme)))]
   [(:seq "---" (:* "-")) 'SEP]
   [(:seq #\" (:* (:~ #\")) #\") 
    (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   ["is" 'IS]
   ["{" 'OPEN]
   ["}" 'CLOSE]
   ["(" 'POPEN]
   [")" 'PCLOSE]
   ["=" 'ASSIGN]
   [(:or "+" "-") (token-UNBINKEYOP (string->symbol lexeme))]
   [(:or "*" "/" "<" ">") (token-BINKEYOP (string->symbol lexeme))]
   [(:or "<=" ">=") (token-BINOP (string->symbol lexeme))]
   ["image" 'IMAGE]
   ["key" 'KEY]
   ["message" 'MESSAGE]
   ["send" 'SEND]
   ["everyone" 'EVERYONE]
   ["move" 'MOVE]
   ["x" 'X]
   ["y" 'Y]
   ["size" 'SIZE]
   ["direction" 'DIRECTION]
   ["change" 'CHANGE]
   ["turn" 'TURN]
   ["to" 'TO]
   ["by" 'BY]
   ["on" 'ON]
   ["do" 'DO]
   ["watch" 'WATCH]
   ["forever" 'FOREVER]
   ["forward" 'FORWARD]
   ["if" 'IF]
   ["while" 'WHILE]
   ["random" 'RANDOM]
   ["variable" 'VARIABLE]
   ["wait" 'WAIT]
   ["say" 'SAY]
   ["hush" 'HUSH]
   ["hide" 'HIDE]
   ["show" 'SHOW]
   ["touches" 'TOUCHES]
   ["use" 'USE]
   [(:seq (:/ #\A #\Z #\a #\z) 
          (:* (:or #\_ (:/ #\A #\Z #\a #\z #\0 #\9))))
    (token-ID (string->symbol lexeme))]
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
    (<prog> [(<uses> <sprites>) (append $1 $2)])
    (<uses> [(<ws>) null]
            [(<ws> USE <ws> ID <uses>) (cons (at-src `(use ,$4)) $5)])
    (<sprites> [(<ws>) null]
               [(<ws> <sprite> <sprites>) (cons $2 $3)])
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
              [(ON <ws> STRING <ws> MESSAGE <ws> <stmts>) (list* (at-src '#:message)
                                                                 $3
                                                                 $7)]
              [(VARIABLE <ws> ID <ws> IS <ws> <expr>) (list (at-src '#:variable)
                                                            $3
                                                            $7)]
              [(DO <ws> <stmts>) (cons (at-src '#:task) $3)])
    (<expr> [(NUM) $1]
            [(ID) $1]
            [(SPECIAL) (at-src `(quote ,$1))]
            [(STRING) (at-src `(quote ,$1))]
            [(RANDOM <ws> <expr>) (at-src `(random ,$3))] 
            [(TOUCHES <ws> <expr>) (at-src `(touches? ,$3))] 
            [(<unop> <ws> <expr>) (at-src `(,$1 ,$3))]
            [(<expr> <ws> <binop> <ws> <expr>) (at-src `(,$3 ,$1 ,$5))]
            [(<expr> <ws> ASSIGN <ws> <expr>) (at-src `(= ,$1 ,$5))]
            [(POPEN <ws> <expr> <ws> PCLOSE) $3])
    (<stmt> [(MOVE <ws> X <ws> BY <ws> <expr>) (at-src `(move-x ,$7))]
            [(MOVE <ws> Y <ws> BY <ws> <expr>) (at-src `(move-y ,$7))]
            [(MOVE <ws> X <ws> TO <ws> <expr>) (at-src `(set-x ,$7))]
            [(MOVE <ws> Y <ws> TO <ws> <expr>) (at-src `(set-y ,$7))]
            [(TURN <ws> TO <ws> <expr>) (at-src `(turn-to ,$5))]
            [(TURN <ws> BY <ws> <expr>) (at-src `(turn ,$5))]
            [(FORWARD <ws> BY <ws> <expr>) (at-src `(forward ,$5))]
            [(CHANGE <ws> SIZE <ws> BY <ws> <expr>) (at-src `(change-size ,$7))]
            [(CHANGE <ws> SIZE <ws> TO <ws> <expr>) (at-src `(set-size ,$7))]
            [(WAIT <ws> <expr>) (at-src `(sleep ,$3))]
            [(FOREVER <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(forever . ,$5))]
            [(IF <ws> <expr> <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(when ,$3 . ,$7))]
            [(WHILE <ws> <expr> <ws> OPEN <ws> <stmts> <ws> CLOSE) (at-src `(while ,$3 . ,$7))]
            [(ID <ws> ASSIGN <ws> <expr>) (at-src `(set! ,$1 ,$5))]
            [(SAY <ws> <expr>) (at-src `(say ,$3))]
            [(HUSH) (at-src `(hush))]
            [(HIDE) (at-src '(hide))]
            [(SHOW) (at-src '(show))]
            [(CHANGE <ws> IMAGE <ws> TO <ws> <expr>) (at-src `(set-image ,$7))]
            [(SAY <ws> <expr>) (at-src `(say ,$3))]
            [(SEND <ws> <expr> <ws> TO <ws> EVERYONE) (at-src `(broadcast ,$3))]
            [(SEND <ws> <expr> <ws> TO <ws> <expr>) (at-src `(tell ,$7 ,$3))]
            [(MOVE <ws> TO <ws> <expr>) (at-src `(set-land ,$5))]
            [(WATCH <ws> <expr>) (at-src `(watch ,$3))])
    (<stmts> [() '()]
             [(<ws> <stmt> <stmts>) (cons $2 $3)])
    (<binop> [(BINOP) $1]
             [(BINKEYOP) $1]
             [(UNBINKEYOP) $1])
    (<unop> [(UNBINKEYOP) $1])
    (<key> [(ID) $1]
           [(X) (at-src 'x)]
           [(Y) (at-src 'y)]
           [(ASSIGN) (at-src '=)]
           [(BINKEYOP) $1]
           [(UNBINKEYOP) $1])
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
