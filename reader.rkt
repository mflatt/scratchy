#lang racket
(require "parser.rkt"
         parser-tools/lex)

(provide read-syntax
         read
         get-info)

;; To read a module:
(define (read-syntax src-name in)
  (define stx (parse src-name in))
  (let* ([p-name (object-name in)]
         [name (if (path? p-name)
                   (let-values ([(base name dir?) (split-path p-name)])
                     (string->symbol
                      (path->string (path-replace-suffix name #""))))
                   'anonymous)])
    (datum->syntax #f `(module ,name scratchy (#%module-begin . ,stx)))))

;; In case `read' is used, instead of `read-syntax':
(define (read in)
  (syntax->datum (read-syntax (object-name in) in)))

;; To get info about the language's environment support:
(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) color-lexer]
      [else default])))

;; Environment support for token coloring:
(define (color-lexer in offset mode)
  ;; Get next token:
  (define tok (lex in))
  ;; Package classification with srcloc:
  (define (ret mode paren [eof? #f])
    (values (if eof?
                eof
                (token->string (position-token-token tok)
                               (token-value (position-token-token tok))))
            mode 
            paren
            (position-offset (position-token-start-pos tok))
            (position-offset (position-token-end-pos tok))
            0 
            #f))
  ;; Convert token to classification:
  (case (token-name (position-token-token tok))
    [(EOF) (ret 'eof #f #t)]
    [(OPEN) (ret 'parenthesis '|{|)]
    [(CLOSE) (ret 'parenthesis '|}|)]
    [(NUM) (ret 'constant #f)]
    [(ID) (ret 'symbol #f)]
    [(WHITESPACE) (ret 'white-space #f)]
    [(SEP) (ret 'parenthesis #f)]
    [(ERROR) (ret 'error #f)]
    [else (ret 'other #f)]))
