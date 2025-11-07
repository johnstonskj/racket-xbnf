#lang info

(define collection "xbnf")
(define pkg-desc "Provides the ability to define a BNF in racket and emit in different syntaxes.")
(define version "1.0")
(define pkg-authors '(johnstonskj))
(define license '(Apache-2.0 OR MIT))

(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/xbnf.scrbl" (multi-page) (library))))
(define test-omit-paths '("scribblings"))
