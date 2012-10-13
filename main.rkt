#lang racket/base
(require "scratchy.rkt")

(provide (all-from-out "scratchy.rkt"))

(module reader racket/base
  (require "reader.rkt")
  (provide (all-from-out "reader.rkt")))
