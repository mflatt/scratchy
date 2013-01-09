#lang racket/base

(module reader syntax/module-reader
  #:language 'scratchy-tutorial/scratchy
  #:read read/exact
  #:read-syntax read-syntax/exact
  (require "read-exact.rkt"))
